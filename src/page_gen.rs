use crate::input::{
    BaseUrlMap, Group, InputData, InputItem, Kind, Mod, Part, References, TraitImplPattern, Type,
};
use crate::parser::{self, ParsedItem};
use crate::token::{Primitive, Range, Token, TokenStream};
use bitflags::bitflags;
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result, Write as _};
use std::fs::File;
use std::io::{self, Write as _};
use std::iter;
use std::path::Path;
use v_htmlescape::escape;

pub fn generate_to(path: impl AsRef<Path>, input: &InputData) -> io::Result<()> {
    let mut file = File::create(path)?;
    let content_writer = PageContentWriter { input };
    write!(
        file,
        include_str!("template.html"),
        title = escape(&input.title),
        content = content_writer
    )?;
    Ok(())
}

struct PageContentWriter<'a> {
    input: &'a InputData,
}

impl Display for PageContentWriter<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let InputData {
            base,
            trait_impls,
            references,
            main,
            ..
        } = self.input;
        Generator::new(base, trait_impls, references).generate(f, main)
    }
}

struct Generator<'a> {
    base: &'a BaseUrlMap,
    trait_impls: Vec<TraitImpl<'a>>,
    references: HashMap<&'a str, Reference<'a>>,
}

struct TraitImpl<'a> {
    pat: TokenStream<'a>,
    generic: Option<&'a str>,
    impls: Vec<TokenStream<'a>>,
}

impl<'a> Generator<'a> {
    fn new(
        base: &'a BaseUrlMap,
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
                iter::empty()
                    .chain(reference.names.iter().map(move |item| {
                        let (path, name) = parse_path(&item);
                        let url = build_type_url(base, &path, kind, name);
                        (name, Reference { kind, path, url })
                    }))
                    .chain(reference.aliases.iter().map(move |(alias, path)| {
                        let (path, name) = parse_path(&path);
                        let url = build_type_url(base, &path, kind, name);
                        (alias.as_str(), Reference { kind, path, url })
                    }))
            })
            .collect();
        Generator {
            base,
            trait_impls,
            references,
        }
    }

    fn generate(&self, f: &mut Formatter, data: &[Vec<Part>]) -> Result {
        write!(f, "<main>")?;
        data.iter()
            .map(|section| self.generate_section(f, &section))
            .collect::<Result>()?;
        write!(f, "</main>")?;
        Ok(())
    }

    fn generate_section(&self, f: &mut Formatter, section: &[Part]) -> Result {
        write!(f, r#"<section class="section">"#)?;
        section
            .iter()
            .map(|part| self.generate_part(f, part))
            .collect::<Result>()?;
        write!(f, "</section>")?;
        Ok(())
    }

    fn generate_part(&self, f: &mut Formatter, part: &Part) -> Result {
        let info = self.build_part_info(part);
        write!(f, r#"<hgroup class="part-title-group">"#)?;
        write!(
            f,
            r#"<h2 class="part-title"><a href="{}">{}</a></h2>"#,
            info.base_url,
            escape(info.title)
        )?;
        if let Some(constraints) = &info.constraints {
            write!(f, r#"<h3 class="part-subtitle">"#)?;
            self.generate_tokens(f, constraints, Flags::LINKIFY | Flags::EXPAND_TRAIT)?;
            write!(f, "</h3>")?;
        }
        write!(f, "</hgroup>")?;
        if let Part::Type(ty) = part {
            if let Some(impls) = &ty.impls {
                self.generate_impls(f, impls)?;
            }
        }
        info.groups
            .iter()
            .map(|group| self.generate_group(f, group, &info))
            .collect::<Result>()
    }

    fn build_part_info(&self, part: &'a Part) -> PartInfo<'a> {
        match part {
            Part::Mod(m) => self.build_part_info_for_mod(m),
            Part::Type(t) => self.build_part_info_for_type(t),
        }
    }

    fn build_part_info_for_mod(&self, m: &'a Mod) -> PartInfo<'a> {
        let path: Vec<_> = m.path.split("::").collect();
        let url = build_path_url(self.base, &path);
        PartInfo {
            title: &m.name,
            base_url: url.into(),
            constraints: None,
            groups: &m.groups,
            fn_type: FunctionType::Function,
        }
    }

    fn build_part_info_for_type(&self, t: &'a Type) -> PartInfo<'a> {
        let ty = parse_type(&t.ty);
        // Unwrap references
        let mut inner = &ty;
        loop {
            let mut iter = inner.0.iter().filter(|token| !token.is_whitespace_only());
            let next_token = match iter.next() {
                Some(Token::Primitive(Primitive::Ref(_))) => iter.next(),
                _ => break,
            };
            inner = match next_token {
                Some(Token::Type(inner)) => inner,
                _ => unreachable!("unexpected token after ref: {:?}", next_token),
            };
        }
        // Use the first token as the source of base url for this part
        let first_token = inner.0.first().expect("empty inner");
        let url = match first_token {
            Token::Identifier(ident) => match self.references.get(ident) {
                Some(r) => r.url.clone(),
                None => unreachable!("unknown name: {}", ident),
            },
            Token::Primitive(primitive) => self.get_primitive_url(primitive),
            _ => unreachable!("unexpected token inside type: {}", first_token),
        };
        let constraints =
            t.constraints.as_ref().map(|constraints| {
                match parser::parse_constraints(constraints.as_str()) {
                    Ok(tokens) => tokens,
                    Err(_) => unreachable!("failed to parse: {}", constraints),
                }
            });
        PartInfo {
            title: &t.ty,
            base_url: url,
            constraints,
            groups: &t.groups,
            fn_type: FunctionType::Method,
        }
    }

    fn generate_impls(&self, f: &mut Formatter, impls: &[String]) -> Result {
        write!(f, r#"<ul class="type-impls">"#)?;
        for impl_item in impls.iter() {
            let parsed = match parser::parse_impl(impl_item) {
                Ok(tokens) => tokens,
                Err(_) => unreachable!("failed to parse impl: {}", impl_item),
            };
            write!(f, "<li>impl ")?;
            self.generate_tokens(f, &parsed, Flags::LINKIFY)?;
            write!(f, "</li>")?;
        }
        write!(f, "</ul>")?;
        Ok(())
    }

    fn generate_group(&self, f: &mut Formatter, group: &Group, part_info: &PartInfo) -> Result {
        if let Some(name) = &group.name {
            write!(f, r#"<h3 class="group-title">{}</h3>"#, escape(name))?;
        }
        write!(f, r#"<ul class="group-list">"#)?;
        group
            .items
            .iter()
            .map(|item| self.generate_item(f, item, part_info))
            .collect::<Result>()?;
        write!(f, "</ul>")?;
        Ok(())
    }

    fn generate_item(&self, f: &mut Formatter, item: &InputItem, part_info: &PartInfo) -> Result {
        let parsed = ParsedItem::parse(item.content())
            .map_err(|_| format!("failed to parse `{}`", item.content()))
            .unwrap();
        let kind = match part_info.fn_type {
            FunctionType::Function => "fn",
            FunctionType::Method => {
                if parsed.takes_self {
                    "method"
                } else {
                    "fn"
                }
            }
        };
        write!(f, r#"<li class="item item-{}">"#, kind)?;
        write!(f, r#"<span class="prefix-fn">fn </span>"#)?;
        let url = match part_info.fn_type {
            FunctionType::Function => format!("fn.{}.html", parsed.name),
            FunctionType::Method => match item.trait_impl() {
                Some(trait_impl) => format!("#impl-{}", escape(trait_impl)),
                None => format!("#method.{}", parsed.name),
            },
        };
        write!(
            f,
            r#"<a href="{}{}" class="{}">{}</a>"#,
            part_info.base_url, url, kind, parsed.name
        )?;
        self.generate_tokens(f, &parsed.tokens, Flags::LINKIFY | Flags::EXPAND_TRAIT)?;
        write!(f, "</li>")?;
        Ok(())
    }

    fn generate_tokens(&self, f: &mut Formatter, tokens: &TokenStream<'_>, flags: Flags) -> Result {
        tokens
            .0
            .iter()
            .map(|token| match token {
                Token::Text(text) => write!(f, "{}", escape(text)),
                Token::Where => write!(f, r#"<span class="where">where</span>"#),
                Token::Identifier(ident) => self.generate_identifier(f, ident, flags),
                Token::AssocType(ty) => write!(f, r#"<span class="assoc-type">{}</span>"#, ty),
                Token::Primitive(primitive) => self.generate_primitive(f, primitive, flags),
                Token::Range(range) => self.generate_range(f, *range, flags),
                Token::Type(ty) => self.generate_type(f, ty, flags),
                Token::Nested(nested) => {
                    write!(f, r#"<span class="nested">"#)?;
                    self.generate_tokens(f, nested, flags)?;
                    write!(f, "</span>")
                }
            })
            .collect()
    }

    fn generate_type(&self, f: &mut Formatter, tokens: &TokenStream<'_>, flags: Flags) -> Result {
        if !flags.contains(Flags::EXPAND_TRAIT) {
            return self.generate_tokens(f, tokens, flags);
        }
        let matched = self.trait_impls.iter().find_map(|trait_impl| {
            match tokens.matches(&trait_impl.pat, trait_impl.generic) {
                Ok(replacement) => Some((trait_impl, replacement)),
                Err(()) => None,
            }
        });
        let (trait_impl, replacement) = match matched {
            Some(matched) => matched,
            None => return self.generate_tokens(f, tokens, flags),
        };
        write!(f, r#"<span class="trait-matched">"#)?;
        self.generate_tokens(f, tokens, flags & !(Flags::LINKIFY | Flags::EXPAND_TRAIT))?;
        write!(f, r#"<aside class="impls">"#)?;
        let flags = flags & !Flags::EXPAND_TRAIT;
        write!(f, r#"<h4 class="impls-title">"#)?;
        self.generate_tokens(f, tokens, flags)?;
        write!(f, r#"</h4><ul class="impls-list">"#)?;
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
                write!(f, "<li>")?;
                self.generate_tokens(f, ty, flags)?;
                write!(f, "</li>")?;
                Ok(())
            })
            .collect::<Result>()?;
        write!(f, "</ul></aside></span>")?;
        Ok(())
    }

    fn generate_identifier(&self, f: &mut Formatter, ident: &str, flags: Flags) -> Result {
        match self.references.get(ident) {
            Some(r) => {
                let kind = r.kind.to_str();
                if flags.contains(Flags::LINKIFY) {
                    write!(f, r#"<a href="{}" class="{}">{}</a>"#, r.url, kind, ident)
                } else {
                    write!(f, r#"<span class="{}">{}</span>"#, kind, ident)
                }
            }
            None => write!(f, "{}", ident),
        }
    }

    fn generate_primitive(
        &self,
        f: &mut Formatter,
        primitive: &Primitive<'_>,
        flags: Flags,
    ) -> Result {
        if flags.contains(Flags::LINKIFY) {
            let url = self.get_primitive_url(primitive);
            write!(
                f,
                r#"<a href="{}" class="primitive">{}</a>"#,
                url, primitive,
            )
        } else {
            write!(f, r#"<span class="primitive">{}</span>"#, primitive)
        }
    }

    fn get_primitive_url(&self, primitive: &Primitive<'_>) -> String {
        let name = match primitive {
            Primitive::SliceStart | Primitive::SliceEnd => "slice",
            Primitive::TupleStart | Primitive::TupleEnd => "tuple",
            Primitive::Unit => "unit",
            Primitive::Ref(_) => "reference",
            Primitive::Named(name) => name,
        };
        let std_url = self.base.get_url_for("std").unwrap();
        format!("{}primitive.{}.html", std_url, name)
    }

    fn generate_range(&self, f: &mut Formatter, range: Range, flags: Flags) -> Result {
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
                f,
                r#"<a href="{}ops/struct.{}.html">{}</a>"#,
                self.base.get_url_for("std").unwrap(),
                name,
                range
            )
        } else {
            write!(f, "{}", range)
        }
    }
}

fn parse_type(ty: &str) -> TokenStream<'_> {
    parser::parse_type(ty)
        .map_err(|_| format!("failed to parse `{}`", ty))
        .unwrap()
}

fn build_type_url(base: &BaseUrlMap, path: &[&str], kind: Kind, name: &str) -> String {
    let mut url = build_path_url(base, path);
    write!(url, "{}.{}.html", kind.to_str(), name).unwrap();
    url
}

fn build_path_url(base: &BaseUrlMap, path: &[&str]) -> String {
    let (crate_name, path) = path.split_first().expect("zero-length path");
    let mut url = base
        .get_url_for(crate_name)
        .expect("unknown crate")
        .to_string();
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
    base_url: String,
    constraints: Option<TokenStream<'a>>,
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
