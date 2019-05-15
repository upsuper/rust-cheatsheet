use crate::input::{Group, InputData, InputItem, Kind, Part, References, TraitImplPattern};
use crate::parser::{self, ParsedItem};
use crate::token::{Primitive, Range, Token, TokenStream};
use bitflags::bitflags;
use std::collections::HashMap;
use std::fmt::Write as _;
use std::fs::File;
use std::io::{self, Write};
use std::marker::PhantomData;
use std::path::Path;
use v_htmlescape::escape;

const STD_URL: &str = "https://doc.rust-lang.org/std/";

type Result = io::Result<()>;

pub fn generate_to(path: impl AsRef<Path>, input: &InputData) -> Result {
    let mut file = File::create(path)?;
    Generator::new(&input.base_url, &input.trait_impls, &input.references)
        .generate(&mut file, &input.main)
}

struct Generator<'a, W> {
    writer_phantom: PhantomData<W>,
    base_url: &'a str,
    trait_impls: Vec<TraitImpl<'a>>,
    references: HashMap<&'a str, Reference<'a>>,
}

struct TraitImpl<'a> {
    pat: TokenStream<'a>,
    generic: Option<&'a str>,
    impls: Vec<TokenStream<'a>>,
}

impl<'a, W> Generator<'a, W>
where
    W: Write,
{
    fn new(
        base_url: &'a str,
        trait_impls: &'a [TraitImplPattern],
        ref_data: &'a [References],
    ) -> Self {
        let trait_impls = trait_impls
            .iter()
            .map(|trait_impl| {
                let pat = parse_type(&trait_impl.pat);
                let generic = trait_impl.generic.as_ref().map(String::as_str);
                let impls = trait_impl.impls.iter().map(|ty| parse_type(ty)).collect();
                TraitImpl {
                    pat,
                    generic,
                    impls,
                }
            })
            .collect();
        let references = ref_data
            .iter()
            .flat_map(|reference| {
                let kind = reference.kind;
                reference.names.iter().map(move |item| {
                    let (path, name) = parse_path(&item);
                    let url = build_type_url(base_url, &path, kind, name);
                    (name, Reference { kind, path, url })
                })
            })
            .collect();
        Generator {
            writer_phantom: PhantomData,
            base_url,
            trait_impls,
            references,
        }
    }

    fn generate(&self, writer: &mut W, data: &[Vec<Part>]) -> Result {
        writer.write_all(include_bytes!("header.html"))?;
        write!(writer, "<main>")?;
        data.iter()
            .map(|section| self.generate_section(writer, &section))
            .collect::<Result>()?;
        write!(writer, "</main>")?;
        writer.write_all(include_bytes!("footer.html"))?;
        Ok(())
    }

    fn generate_section(&self, writer: &mut W, section: &[Part]) -> Result {
        write!(writer, "<section>")?;
        let mut last_kind = None;
        let mut last_path = None;
        section
            .iter()
            .map(|part| {
                let info = self.build_part_info(part, &mut last_kind, &mut last_path);
                write!(
                    writer,
                    r#"<h2><a href="{}">{}</a></h2>"#,
                    info.url,
                    escape(info.title)
                )?;
                info.groups
                    .iter()
                    .map(|group| self.generate_group(writer, group, &info))
                    .collect::<Result>()
            })
            .collect::<Result>()?;
        write!(writer, "</section>")?;
        Ok(())
    }

    fn generate_group(&self, writer: &mut W, group: &Group, part_info: &PartInfo) -> Result {
        write!(writer, "<h3>{}</h3>", escape(&group.name))?;
        write!(writer, "<ul>")?;
        group
            .items
            .iter()
            .map(|item| self.generate_item(writer, item, part_info))
            .collect::<Result>()?;
        write!(writer, "</ul>")?;
        Ok(())
    }

    fn generate_item(&self, writer: &mut W, item: &InputItem, part_info: &PartInfo) -> Result {
        let parsed = ParsedItem::parse(item.content())
            .map_err(|_| format!("failed to parse `{}`", item.content()))
            .unwrap();
        let kind = match part_info.fn_type {
            FunctionType::Function => "fn",
            FunctionType::Method => match parsed.takes_self {
                true => "method",
                false => "fn",
            },
        };
        write!(writer, r#"<li class="item item-{}">"#, kind)?;
        write!(writer, r#"<span class="prefix-fn">fn </span>"#)?;
        let url = match part_info.fn_type {
            FunctionType::Function => format!("fn.{}.html", parsed.name),
            FunctionType::Method => match item.trait_impl() {
                Some(trait_impl) => format!("#impl-{}", escape(trait_impl)),
                None => format!("#method.{}", parsed.name),
            },
        };
        write!(
            writer,
            r#"<a href="{}{}" class="{}">{}</a>"#,
            part_info.url, url, kind, parsed.name
        )?;
        self.generate_tokens(writer, &parsed.tokens, Flags::LINKIFY | Flags::EXPAND_TRAIT)?;
        write!(writer, "</li>")?;
        Ok(())
    }

    fn generate_tokens(&self, writer: &mut W, tokens: &TokenStream<'_>, flags: Flags) -> Result {
        tokens
            .0
            .iter()
            .map(|token| match token {
                Token::Text(text) => write!(writer, "{}", escape(text)),
                Token::Where => write!(writer, r#"<span class="where">where</span>"#),
                Token::Identifier(ident) => self.generate_identifier(writer, ident, flags),
                Token::AssocType(ty) => write!(writer, r#"<span class="assoc-type">{}</span>"#, ty),
                Token::Primitive(primitive) => self.generate_primitive(writer, primitive, flags),
                Token::Range(range) => self.generate_range(writer, *range, flags),
                Token::Type(ty) => self.generate_type(writer, ty, flags),
                Token::Nested(nested) => {
                    write!(writer, r#"<span class="nested">"#)?;
                    self.generate_tokens(writer, nested, flags)?;
                    write!(writer, "</span>")
                }
            })
            .collect()
    }

    fn generate_type(&self, writer: &mut W, tokens: &TokenStream<'_>, flags: Flags) -> Result {
        if !flags.contains(Flags::EXPAND_TRAIT) {
            return self.generate_tokens(writer, tokens, flags);
        }
        let matched = self.trait_impls.iter().find_map(|trait_impl| {
            match tokens.matches(&trait_impl.pat, trait_impl.generic) {
                Ok(replacement) => Some((trait_impl, replacement)),
                Err(()) => None,
            }
        });
        let (trait_impl, replacement) = match matched {
            Some(matched) => matched,
            None => return self.generate_tokens(writer, tokens, flags),
        };
        write!(writer, r#"<span class="trait-matched">"#)?;
        self.generate_tokens(
            writer,
            tokens,
            flags & !(Flags::LINKIFY | Flags::EXPAND_TRAIT),
        )?;
        write!(writer, r#"<aside class="impls">"#)?;
        let flags = flags & !Flags::EXPAND_TRAIT;
        write!(writer, "<h4>")?;
        self.generate_tokens(writer, tokens, flags)?;
        write!(writer, "</h4><ul>")?;
        trait_impl
            .impls
            .iter()
            .map(|ty| {
                let replaced = match (trait_impl.generic, replacement) {
                    (Some(generic), Some(replacement)) => {
                        Some(build_tokens_with_replacement(ty, generic, replacement))
                    }
                    _ => None,
                };
                let ty = replaced.as_ref().unwrap_or(ty);
                write!(writer, "<li>")?;
                self.generate_tokens(writer, ty, flags)?;
                write!(writer, "</li>")?;
                Ok(())
            })
            .collect::<Result>()?;
        write!(writer, "</ul></aside></span>")?;
        Ok(())
    }

    fn generate_identifier(&self, writer: &mut W, ident: &str, flags: Flags) -> Result {
        match self.references.get(ident) {
            Some(r) => {
                let kind = r.kind.to_str();
                if flags.contains(Flags::LINKIFY) {
                    write!(
                        writer,
                        r#"<a href="{}" class="{}">{}</a>"#,
                        r.url, kind, ident
                    )
                } else {
                    write!(writer, r#"<span class="{}">{}</span>"#, kind, ident)
                }
            }
            None => write!(writer, "{}", ident),
        }
    }

    fn generate_primitive(
        &self,
        writer: &mut W,
        primitive: &Primitive<'_>,
        flags: Flags,
    ) -> Result {
        if flags.contains(Flags::LINKIFY) {
            let name = match primitive {
                Primitive::SliceStart | Primitive::SliceEnd => "slice",
                Primitive::TupleStart | Primitive::TupleEnd => "tuple",
                Primitive::Unit => "unit",
                Primitive::Ref(_) => "reference",
                Primitive::Named(name) => name,
            };
            write!(
                writer,
                r#"<a href="{}primitive.{}.html" class="primitive">{}</a>"#,
                STD_URL, name, primitive
            )
        } else {
            write!(writer, r#"<span class="primitive">{}</span>"#, primitive)
        }
    }

    fn generate_range(&self, writer: &mut W, range: Range, flags: Flags) -> Result {
        if flags.contains(Flags::LINKIFY) {
            let name = match range {
                Range::Range => "Range",
                Range::RangeFrom => "RangeFrom",
                Range::RangeFull => "RangeFull",
                Range::RangeInclusive => "RangeInclusive",
                Range::RangeTo => "RangeTo",
                Range::RangeToInclusive => "RangeToInclusive",
            };
            write!(
                writer,
                r#"<a href="{}ops/struct.{}.html">{}</a>"#,
                STD_URL, name, range
            )
        } else {
            write!(writer, "{}", range)
        }
    }

    fn build_part_info<'p>(
        &self,
        part: &'p Part,
        last_kind: &mut Option<Kind>,
        last_path: &mut Option<&'p str>,
    ) -> PartInfo<'p> {
        match part {
            Part::Mod(m) => {
                *last_kind = None;
                *last_path = None;
                let path: Vec<_> = m.path.split("::").collect();
                PartInfo {
                    title: &m.name,
                    url: build_path_url(self.base_url, &path),
                    groups: &m.groups,
                    fn_type: FunctionType::Function,
                }
            }
            Part::Type(t) => {
                *last_kind = t.kind.or(*last_kind);
                *last_path = t.path.as_ref().map(String::as_str).or(*last_path);
                let kind = last_kind.expect("expect kind");
                let path = last_path.expect("expect path");
                let (path, name) = parse_path(path);
                PartInfo {
                    title: &t.ty,
                    url: build_type_url(self.base_url, &path, kind, name),
                    groups: &t.groups,
                    fn_type: FunctionType::Method,
                }
            }
        }
    }
}

fn parse_type(ty: &str) -> TokenStream<'_> {
    parser::parse_type(ty)
        .map_err(|_| format!("failed to parse `{}`", ty))
        .unwrap()
}

fn build_type_url(base: &str, path: &[&str], kind: Kind, name: &str) -> String {
    let mut url = build_path_url(base, path);
    write!(url, "{}.{}.html", kind.to_str(), name).unwrap();
    url
}

fn build_path_url(base: &str, path: &[&str]) -> String {
    let mut url = base.to_string();
    for s in path.iter() {
        url.push_str(s);
        url.push('/');
    }
    url
}

fn build_tokens_with_replacement<'a>(
    tokens: &'a TokenStream<'a>,
    generic: &str,
    replacement: &'a TokenStream<'a>,
) -> TokenStream<'a> {
    tokens
        .0
        .iter()
        .map(|token| match token {
            Token::Type(nested) => Token::Type(match nested.0.as_slice() {
                [Token::Identifier(ident)] if *ident == generic => replacement.clone(),
                _ => build_tokens_with_replacement(nested, generic, replacement),
            }),
            Token::Nested(nested) => {
                Token::Nested(build_tokens_with_replacement(nested, generic, replacement))
            }
            _ => token.clone(),
        })
        .collect()
}

#[derive(Debug)]
struct Reference<'a> {
    kind: Kind,
    path: Box<[&'a str]>,
    url: String,
}

struct PartInfo<'a> {
    title: &'a str,
    url: String,
    groups: &'a [Group],
    fn_type: FunctionType,
}

fn parse_path(s: &str) -> (Box<[&str]>, &str) {
    let mut path: Vec<_> = s.split("::").collect();
    let name = path.pop().unwrap();
    let path = path.into_boxed_slice();
    (path, name)
}

#[derive(Clone, Copy)]
enum FunctionType {
    Function,
    Method,
}

bitflags! {
    struct Flags: u8 {
        /// Linkify identifiers and symbols when possible
        const LINKIFY = 0b0001;
        /// Expand trait to list of types when available
        const EXPAND_TRAIT = 0b0010;
    }
}
