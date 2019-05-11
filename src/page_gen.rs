use crate::input::{Group, InputData, InputItem, Kind, MainData, Part, ReferenceData};
use crate::parser::parse_item;
use crate::token::{Primitive, Range, Token, TokenStream};
use std::collections::HashMap;
use std::fmt::Write as _;
use std::fs::File;
use std::io::{self, Write};
use std::path::Path;
use v_htmlescape::escape;

const STD_URL: &str = "https://doc.rust-lang.org/std/";

type Result = io::Result<()>;

pub fn generate_to(path: impl AsRef<Path>, input: &InputData) -> Result {
    let file = File::create(path)?;
    Generator::new(file, &input.base_url, &input.references).generate(&input.main)
}

struct Generator<'a, W> {
    writer: W,
    base_url: &'a str,
    references: HashMap<&'a str, Reference<'a>>,
}

impl<'a, W> Generator<'a, W>
where
    W: Write,
{
    fn new(writer: W, base_url: &'a str, ref_data: &'a ReferenceData) -> Self {
        let references = ref_data
            .0
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
            writer,
            base_url,
            references,
        }
    }

    fn generate(&mut self, data: &MainData) -> Result {
        self.writer.write_all(include_bytes!("header.html"))?;
        write!(self.writer, "<main>")?;
        data.0
            .iter()
            .map(|section| self.generate_section(&section))
            .collect::<Result>()?;
        write!(self.writer, "</main>")?;
        self.writer.write_all(include_bytes!("footer.html"))?;
        Ok(())
    }

    fn generate_section(&mut self, section: &[Part]) -> Result {
        write!(self.writer, "<section>")?;
        let mut last_kind = None;
        let mut last_path = None;
        section
            .iter()
            .map(|part| {
                let info = self.build_part_info(part, &mut last_kind, &mut last_path);
                write!(
                    self.writer,
                    r#"<h2><a href="{}">{}</a></h2>"#,
                    info.url,
                    escape(info.title)
                )?;
                info.groups
                    .iter()
                    .map(|group| self.generate_group(group, &info))
                    .collect::<Result>()
            })
            .collect::<Result>()?;
        write!(self.writer, "</section>")?;
        Ok(())
    }

    fn generate_group(&mut self, group: &Group, part_info: &PartInfo) -> Result {
        write!(self.writer, "<h3>{}</h3>", escape(&group.name))?;
        write!(self.writer, "<ul>")?;
        group
            .items
            .iter()
            .map(|item| self.generate_item(item, part_info))
            .collect::<Result>()?;
        write!(self.writer, "</ul>")?;
        Ok(())
    }

    fn generate_item(&mut self, item: &InputItem, part_info: &PartInfo) -> Result {
        let kind = match part_info.fn_type {
            FunctionType::Function => "fn",
            FunctionType::Method => "method",
        };
        write!(self.writer, r#"<li class="item-{}">"#, kind)?;
        write!(self.writer, r#"<span class="prefix-fn">fn </span>"#)?;
        let (name, tokens) = parse_item(item.content())
            .map_err(|_| format!("failed to parse `{}`", item.content()))
            .unwrap();
        let url = match part_info.fn_type {
            FunctionType::Function => format!("fn.{}.html", name),
            FunctionType::Method => match item.trait_impl() {
                Some(trait_impl) => format!("#impl-{}", escape(trait_impl)),
                None => format!("#method.{}", name),
            },
        };
        write!(
            self.writer,
            r#"<a href="{}{}" class="{}">{}</a>"#,
            part_info.url, url, kind, name
        )?;
        self.generate_tokens(tokens)?;
        write!(self.writer, "</li>")?;
        Ok(())
    }

    fn generate_tokens(&mut self, tokens: TokenStream<'_>) -> Result {
        tokens
            .0
            .into_iter()
            .map(|token| match token {
                Token::Text(text) => write!(self.writer, "{}", escape(text)),
                Token::Where => write!(self.writer, r#"<span class="where">where</span>"#),
                Token::Identifier(ident) => self.generate_identifier(ident),
                Token::AssocType(ty) => {
                    write!(self.writer, r#"<span class="assoc-type">{}</span>"#, ty)
                }
                Token::Primitive(primitive) => self.generate_primitive(primitive),
                Token::Range(range) => self.generate_range(range),
                Token::Nested(nested) => {
                    write!(self.writer, r#"<span class="nested">"#)?;
                    self.generate_tokens(nested)?;
                    write!(self.writer, "</span>")
                }
            })
            .collect()
    }

    fn generate_identifier(&mut self, ident: &str) -> Result {
        match self.references.get(ident) {
            Some(r) => write!(
                self.writer,
                r#"<a href="{url}" class="{class}">{ident}</a>"#,
                url = r.url,
                class = r.kind.to_str(),
                ident = ident,
            ),
            None => write!(self.writer, "{}", ident),
        }
    }

    fn generate_primitive(&mut self, primitive: Primitive<'_>) -> Result {
        let name = match primitive {
            Primitive::SliceStart | Primitive::SliceEnd => "slice",
            Primitive::TupleStart | Primitive::TupleEnd => "tuple",
            Primitive::Unit => "unit",
            Primitive::Ref(_) => "reference",
            Primitive::Named(name) => name,
        };
        write!(
            self.writer,
            r#"<a href="{std}primitive.{name}.html" class="primitive">{text}</a>"#,
            std = STD_URL,
            name = name,
            text = primitive,
        )
    }

    fn generate_range(&mut self, range: Range) -> Result {
        let name = match range {
            Range::Range => "Range",
            Range::RangeFrom => "RangeFrom",
            Range::RangeFull => "RangeFull",
            Range::RangeInclusive => "RangeInclusive",
            Range::RangeTo => "RangeTo",
            Range::RangeToInclusive => "RangeToInclusive",
        };
        write!(
            self.writer,
            r#"<a href="{std}ops/struct.{name}.html">{range}</a>"#,
            std = STD_URL,
            name = name,
            range = range
        )
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
