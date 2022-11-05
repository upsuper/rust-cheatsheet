use std::fmt::{self, Display, Write as _};
use std::iter::FromIterator;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct TokenStream<'a>(pub Vec<Token<'a>>);

impl<'a> TokenStream<'a> {
    pub fn matches(
        &'a self,
        pat: &TokenStream<'_>,
        generic: Option<&str>,
    ) -> Result<Option<&'a TokenStream<'a>>, ()> {
        let mut replacement = None;
        if tokens_match(self, pat, generic, &mut replacement) {
            Ok(replacement)
        } else {
            Err(())
        }
    }
}

fn tokens_match<'a>(
    tokens: &'a TokenStream<'a>,
    pat: &TokenStream<'_>,
    generic: Option<&str>,
    replacement: &mut Option<&'a TokenStream<'a>>,
) -> bool {
    tokens
        .0
        .iter()
        .zip(pat.0.iter())
        .all(|(token, pat)| match (token, pat) {
            (Token::Where, Token::Where) => true,
            (Token::Identifier(this), Token::Identifier(pat)) => this == pat,
            (Token::Primitive(this), Token::Primitive(pat)) => this == pat,
            (Token::Range(this), Token::Range(pat)) => this == pat,
            (Token::AssocType(this), Token::AssocType(pat)) => this == pat,
            (Token::Nested(this), Token::Nested(pat)) => {
                tokens_match(this, pat, generic, replacement)
            }
            (Token::Text(this), Token::Text(pat)) => this
                .split_ascii_whitespace()
                .zip(pat.split_ascii_whitespace())
                .all(|(this, pat)| this == pat),
            (Token::Type(this), Token::Type(pat)) => match (pat.0.as_slice(), generic) {
                ([Token::Identifier(ident)], Some(generic)) if *ident == generic => {
                    if let Some(replacement) = replacement {
                        tokens_match(this, replacement, None, &mut None)
                    } else {
                        *replacement = Some(this);
                        true
                    }
                }
                _ => tokens_match(this, pat, generic, replacement),
            },
            _ => false,
        })
}

impl Display for TokenStream<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.iter().try_for_each(|token| write!(f, "{}", token))
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
    Range(RangeToken),
    Where,
}

impl Token<'_> {
    pub fn is_whitespace_only(&self) -> bool {
        match self {
            Token::Text(text) => text.trim().is_empty(),
            _ => false,
        }
    }
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
    Ptr(&'a str),
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
            Primitive::Ref(s) | Primitive::Ptr(s) | Primitive::Named(s) => f.write_str(s),
            Primitive::SliceStart => f.write_char('['),
            Primitive::SliceEnd => f.write_char(']'),
            Primitive::TupleStart => f.write_char('('),
            Primitive::TupleEnd => f.write_char(')'),
            Primitive::Unit => f.write_str("()"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RangeToken {
    Range,
    RangeFrom,
    RangeFull,
    RangeInclusive,
    RangeTo,
    RangeToInclusive,
}

impl Display for RangeToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(match self {
            RangeToken::Range | RangeToken::RangeFrom | RangeToken::RangeFull | RangeToken::RangeTo => "..",
            RangeToken::RangeInclusive | RangeToken::RangeToInclusive => "..=",
        })
    }
}

#[cfg(test)]
mod tests {
    use super::Token;
    use crate::parser::parse_type;

    #[test]
    fn token_stream_matches() {
        fn check_match(
            pat: &str,
            generic: Option<&str>,
            cases: &[(&str, Result<Option<&[Token<'_>]>, ()>)],
        ) {
            let pat = parse_type(pat).unwrap();
            for (ty, expected) in cases.iter() {
                let ty = parse_type(ty).unwrap();
                let actual = ty.matches(&pat, generic);
                let expected = match expected {
                    Ok(Some([Token::Type(tokens)])) => Ok(Some(tokens)),
                    Ok(None) => Ok(None),
                    Err(()) => Err(()),
                    _ => unreachable!("unexpected `expected`: `{:?}`", expected),
                };
                assert_eq!(actual, expected);
            }
        }
        check_match(
            "Try<Ok = T>",
            Some("T"),
            &[
                ("Try <Ok=usize>", Ok(Some(&tokens!(@usize)))),
                (
                    "Try <Ok= Option<T> >",
                    Ok(Some(&tokens!(^[Option "<" ^T ">"]))),
                ),
                (
                    "Try<Ok= () -> Option<T> >",
                    Ok(Some(&tokens!(^["(" ") -> " ^[Option "<" ^T ">"]]))),
                ),
                ("Try<Err = T>", Err(())),
                ("Result<Ok = T>", Err(())),
                ("&Try<Ok = T>", Err(())),
            ],
        );
        check_match(
            "SliceIndex<[T]>",
            Some("T"),
            &[
                ("SliceIndex<[usize]>", Ok(Some(&tokens!(@usize)))),
                ("SliceIndex<[()]>", Ok(Some(&tokens!(@())))),
                ("SliceIndex<[[T]]>", Ok(Some(&tokens!(@[^T])))),
                ("SliceIndex<T>", Err(())),
            ],
        );
        check_match(
            "RangeBounds<usize>",
            None,
            &[
                ("RangeBounds<usize>", Ok(None)),
                ("RangeBounds < usize >", Ok(None)),
                ("RangeBounds<u8>", Err(())),
            ],
        );
    }
}
