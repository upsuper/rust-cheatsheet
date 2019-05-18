use serde::Deserialize;
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::path::Path;

const DEFAULT_STD_URL: &str = "https://doc.rust-lang.org/std/";

#[derive(Debug, Deserialize)]
pub struct InputData {
    pub title: String,
    #[serde(default)]
    pub base: BaseUrlMap,
    pub main: Vec<Vec<Part>>,
    #[serde(default)]
    pub trait_impls: Vec<TraitImplPattern>,
    pub references: Vec<References>,
}

impl InputData {
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self, Box<dyn Error>> {
        let file = File::open(path)?;
        Ok(serde_yaml::from_reader(file)?)
    }
}

#[derive(Debug, Default, Deserialize)]
pub struct BaseUrlMap(HashMap<String, String>);

impl BaseUrlMap {
    pub fn get_url_for(&self, name: &str) -> Option<&str> {
        self.0.get(name).map(String::as_str).or_else(|| match name {
            "std" => Some(DEFAULT_STD_URL),
            _ => None,
        })
    }
}

// TODO: try to avoid using untagged here
// because untagged makes it hard to debug data file when parsing fails.
// Also see https://github.com/serde-rs/serde/issues/1520
#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Part {
    Mod(Mod),
    Type(Type),
}

#[derive(Debug, Deserialize)]
pub struct Mod {
    #[serde(rename = "mod")]
    pub name: String,
    pub path: String,
    pub groups: Vec<Group>,
}

#[derive(Debug, Deserialize)]
pub struct Type {
    #[serde(rename = "type")]
    pub ty: String,
    pub constraints: Option<String>,
    pub impls: Option<Vec<String>>,
    pub groups: Vec<Group>,
}

#[derive(Clone, Copy, Debug, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Kind {
    Enum,
    Primitive,
    Struct,
    Trait,
    Type,
}

impl Kind {
    pub fn to_str(self) -> &'static str {
        match self {
            Kind::Enum => "enum",
            Kind::Primitive => "primitive",
            Kind::Struct => "struct",
            Kind::Trait => "trait",
            Kind::Type => "type",
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct Group {
    pub name: Option<String>,
    pub items: Vec<InputItem>,
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum InputItem {
    Plain(String),
    Detailed {
        trait_impl: Option<String>,
        content: String,
    },
}

impl InputItem {
    pub fn content(&self) -> &str {
        match self {
            InputItem::Plain(content) => content.as_str(),
            InputItem::Detailed { content, .. } => content.as_str(),
        }
    }

    pub fn trait_impl(&self) -> Option<&str> {
        match self {
            InputItem::Plain(_) => None,
            InputItem::Detailed { trait_impl, .. } => trait_impl.as_ref().map(String::as_str),
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct TraitImplPattern {
    pub pat: String,
    pub generic: Option<String>,
    pub impls: Vec<String>,
}

#[derive(Debug, Deserialize)]
pub struct References {
    pub kind: Kind,
    pub names: Vec<String>,
}
