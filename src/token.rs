use std::fmt::{self, Display, Write as _};
use std::iter::FromIterator;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct TokenStream<'a>(pub Vec<Token<'a>>);

impl Display for TokenStream<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.iter().map(|token| write!(f, "{}", token)).collect()
    }
}

impl<'a> FromIterator<Token<'a>> for TokenStream<'a> {
    fn from_iter<I: IntoIterator<Item = Token<'a>>>(iter: I) -> Self {
        TokenStream(Vec::from_iter(iter))
    }
}

impl<'a> IntoIterator for TokenStream<'a> {
    type Item = Token<'a>;
    type IntoIter = <Vec<Token<'a>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a> Extend<Token<'a>> for TokenStream<'a> {
    fn extend<I: IntoIterator<Item = Token<'a>>>(&mut self, iter: I) {
        self.0.extend(iter);
    }
}

impl<'a, Iter> Extend<Iter> for TokenStream<'a>
where
    Iter: IntoIterator<Item = Token<'a>>,
{
    fn extend<I: IntoIterator<Item = Iter>>(&mut self, iter: I) {
        self.0.extend(iter.into_iter().flatten())
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token<'a> {
    Text(&'a str),
    Nested(TokenStream<'a>),
    Type(TokenStream<'a>),
    Primitive(Primitive<'a>),
    Identifier(&'a str),
    AssocType(&'a str),
    Range(Range),
    Where,
}

impl<'a> From<Primitive<'a>> for Token<'a> {
    fn from(primitive: Primitive<'a>) -> Self {
        Token::Primitive(primitive)
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Text(s) | Token::Identifier(s) | Token::AssocType(s) => f.write_str(s),
            Token::Nested(inner) | Token::Type(inner) => write!(f, "{}", inner),
            Token::Primitive(p) => write!(f, "{}", p),
            Token::Range(r) => write!(f, "{}", r),
            Token::Where => f.write_str("where"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Primitive<'a> {
    Ref(&'a str),
    SliceStart,
    SliceEnd,
    TupleStart,
    TupleEnd,
    Unit,
    Named(&'a str),
}

impl Display for Primitive<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Primitive::Ref(s) | Primitive::Named(s) => f.write_str(s),
            Primitive::SliceStart => f.write_char('['),
            Primitive::SliceEnd => f.write_char(']'),
            Primitive::TupleStart => f.write_char('('),
            Primitive::TupleEnd => f.write_char(')'),
            Primitive::Unit => f.write_str("()"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Range {
    Range,
    RangeFrom,
    RangeFull,
    RangeInclusive,
    RangeTo,
    RangeToInclusive,
}

impl Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            Range::Range | Range::RangeFrom | Range::RangeFull | Range::RangeTo => "..",
            Range::RangeInclusive | Range::RangeToInclusive => "..=",
        })
    }
}
