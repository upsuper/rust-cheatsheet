use crate::token::{Primitive, Range, Token, TokenStream};
use combine::error::StringStreamError;
use combine::parser::{
    char::{alpha_num, char, letter, space, spaces, string},
    choice::{choice, optional},
    combinator::attempt,
    range::recognize,
    repeat::{many, skip_many1},
    Parser,
};
use either_n::{Either2, Either3, Either6};
use std::iter;

pub fn parse_item(input: &str) -> Result<(&str, TokenStream<'_>), ()> {
    (identifier_str(), item_after_name())
        .parse(input)
        .map_err(|_| ())
        .and_then(|((name, rest), remaining)| match remaining {
            "" => Ok((name, TokenStream(rest.collect()))),
            _ => Err(()),
        })
}

// TODO: Replace this macro with named existential type when it's available.
// See https://github.com/rust-lang/rust/issues/34511
macro_rules! parser_str_to_iter_token {
    ($a:lifetime) => {
        parser_str_to!($a, impl Iterator<Item = Token<$a>>)
    };
}

macro_rules! parser_str_to {
    ($a:lifetime, $ty:ty) => {
        impl Parser<Input = &$a str, Output = $ty>
    }
}

fn item_after_name<'a>() -> parser_str_to_iter_token!('a) {
    (
        lex("("),
        nested_type_like_list(),
        lex(")"),
        optional_tokens(chain2(lex("->"), single_type_like())),
        optional_tokens(chain4(
            wrap("where", Token::Where),
            single_type_like(),
            lex(":"),
            sep1_by_lex(single_type_like, "+"),
        )),
    )
        .map(|(left, params, right, ret, where_clause)| {
            iter::empty()
                .chain(left)
                .chain(params)
                .chain(right)
                .chain(ret)
                .chain(where_clause)
        })
}

type BoxedTokenIter<'a> = Box<dyn Iterator<Item = Token<'a>> + 'a>;

// Add an extra wrapper for this parser so that it can be invoked recursively.
parser! {
    fn type_like['a]()(&'a str) -> BoxedTokenIter<'a> {
        type_like_inner()
    }
}

fn type_like_inner<'a>() -> parser_str_to!('a, BoxedTokenIter<'a>) {
    sep1_by_lex(single_type_like, "|").map(to_boxed_iter)
}

fn to_boxed_iter<'a, T>(iter: impl Iterator<Item = T> + 'a) -> Box<dyn Iterator<Item = T> + 'a> {
    Box::new(iter)
}

fn single_type_like<'a>() -> parser_str_to_iter_token!('a) {
    choice((
        attempt(ref_type()).map(Either6::One),
        attempt(slice_type()).map(Either6::Two),
        attempt(fn_type()).map(Either6::Three),
        attempt(tuple_type()).map(Either6::Four),
        attempt(range_type()).map(Either6::Five),
        named_type().map(Either6::Six),
    ))
}

fn ref_type<'a>() -> parser_str_to_iter_token!('a) {
    chain3(
        recognize((
            char('&'),
            optional(string("mut")),
            optional(attempt((spaces(), lifetime()))),
        ))
        .map(|s| iter::once(Token::Primitive(Primitive::Ref(s)))),
        maybe_spaces(),
        type_like(),
    )
}

fn slice_type<'a>() -> parser_str_to_iter_token!('a) {
    chain3(
        wrap("[", Primitive::SliceStart),
        type_like(),
        wrap("]", Primitive::SliceEnd),
    )
}

fn fn_type<'a>() -> parser_str_to_iter_token!('a) {
    chain4(
        lex("("),
        nested_type_like_list(),
        text((spaces(), char(')'), spaces(), string("->"), spaces())),
        type_like(),
    )
}

fn tuple_type<'a>() -> parser_str_to_iter_token!('a) {
    choice((
        attempt(wrap("()", Primitive::Unit)).map(Either2::One),
        chain3(
            wrap("(", Primitive::TupleStart),
            nested_type_like_list(),
            wrap(")", Primitive::TupleEnd),
        )
        .map(Either2::Two),
    ))
}

fn nested_type_like_list<'a>() -> parser_str_to_iter_token!('a) {
    optional(
        sep1_by_lex(type_like, ",")
            .map(Iterator::collect)
            .map(Token::Nested),
    )
    .map(IntoIterator::into_iter)
}

#[rustfmt::skip]
fn range_type<'a>() -> parser_str_to_iter_token!('a) {
    (
        optional(named_type()),
        choice((attempt(lex_str("..=")), attempt(lex_str("..")))),
        optional(named_type()),
    )
        .and_then(|(start, op, end)| {
            Ok(match (start, op.trim(), end) {
                (None, "..", None) => {
                    Either6::One(range_token(op, Range::RangeFull))
                },
                (None, "..", Some(end)) => {
                    Either6::Two(range_token(op, Range::RangeTo).chain(end))
                },
                (None, "..=", Some(end)) => {
                    Either6::Three(range_token(op, Range::RangeToInclusive).chain(end))
                }
                (Some(start), "..", None) => {
                    Either6::Four(start.chain(range_token(op, Range::RangeFrom)))
                }
                (Some(start), "..", Some(end)) => {
                    Either6::Five(start.chain(range_token(op, Range::Range)).chain(end))
                }
                (Some(start), "..=", Some(end)) => {
                    Either6::Six(start.chain(range_token(op, Range::RangeInclusive)).chain(end))
                },
                _ => return Err(StringStreamError::UnexpectedParse),
            })
        })
}

fn range_token(s: &str, range: Range) -> impl Iterator<Item = Token<'_>> {
    let start = match &s[..s.len() - s.trim_start().len()] {
        "" => None,
        spaces => Some(Token::Text(spaces)),
    };
    let end = match &s[s.trim_end().len()..] {
        "" => None,
        spaces => Some(Token::Text(spaces)),
    };
    iter::empty()
        .chain(start)
        .chain(iter::once(Token::Range(range)))
        .chain(end)
}

fn named_type<'a>() -> parser_str_to_iter_token!('a) {
    chain4(
        // Optional `dyn` keyword
        optional_tokens(text((string("dyn"), skip_many1(space())))),
        // Name
        identifier_str().map(|ident| {
            iter::once(if is_primitive(ident) {
                Token::Primitive(Primitive::Named(ident))
            } else {
                Token::Identifier(ident)
            })
        }),
        // Optional parameters
        optional_tokens(chain3(lex("<"), sep1_by_lex(type_param, ","), lex(">"))),
        // Associated items
        many::<TokenStream<'_>, _>(attempt(chain2(
            lex("::"),
            identifier_str().map(Token::AssocType).map(iter::once),
        ))),
    )
}

#[rustfmt::skip]
fn is_primitive(ident: &str) -> bool {
    match ident {
        "bool" | "char" | "str" |
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" |
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" => true,
        _ => false,
    }
}

fn type_param<'a>() -> parser_str_to_iter_token!('a) {
    choice((
        attempt(lifetime_param()).map(Either3::One),
        attempt(assoc_type_param()).map(Either3::Two),
        type_like().map(Either3::Three),
    ))
}

fn lifetime_param<'a>() -> parser_str_to_iter_token!('a) {
    text(lifetime())
}

fn assoc_type_param<'a>() -> parser_str_to_iter_token!('a) {
    chain3(
        identifier_str().map(Token::AssocType).map(iter::once),
        lex("="),
        type_like(),
    )
}

fn optional_tokens<'a>(inner: parser_str_to_iter_token!('a)) -> parser_str_to_iter_token!('a) {
    optional(attempt(inner))
        .map(IntoIterator::into_iter)
        .map(Iterator::flatten)
}

fn sep1_by_lex<'a, P, I>(
    parser_fn: impl Fn() -> P,
    sep: &'static str,
) -> parser_str_to_iter_token!('a)
where
    P: Parser<Input = &'a str, Output = I>,
    I: Iterator<Item = Token<'a>>,
{
    chain2(
        parser_fn(),
        many::<TokenStream<'a>, _>(attempt(chain2(lex(sep), parser_fn()))),
    )
}

fn lex<'a>(s: &'static str) -> parser_str_to_iter_token!('a) {
    text(lex_str(s))
}

fn lex_str<'a>(s: &'static str) -> parser_str_to!('a, &'a str) {
    recognize((spaces(), string(s), spaces()))
}

fn wrap<'a>(inner: &'static str, token: impl Into<Token<'a>>) -> parser_str_to_iter_token!('a) {
    let token = token.into();
    chain3(
        maybe_spaces(),
        string(inner).map(move |_| iter::once(token.clone())),
        maybe_spaces(),
    )
}

fn maybe_spaces<'a>() -> parser_str_to_iter_token!('a) {
    recognize(spaces()).map(|s| match s {
        "" => None.into_iter(),
        s => Some(Token::Text(s)).into_iter(),
    })
}

fn text<'a>(inner: impl Parser<Input = &'a str>) -> parser_str_to_iter_token!('a) {
    text_token(inner).map(iter::once)
}

fn text_token<'a>(
    inner: impl Parser<Input = &'a str>,
) -> impl Parser<Input = &'a str, Output = Token<'a>> {
    recognize(inner).map(Token::Text)
}

fn lifetime<'a>() -> parser_str_to!('a, &'a str) {
    recognize((char('\''), skip_many1(letter())))
}

fn identifier_str<'a>() -> parser_str_to!('a, &'a str) {
    recognize(skip_many1(choice((alpha_num(), char('_')))))
}

macro_rules! impl_chain {
    ($name:ident: $($v:ident)+) => {
        fn $name<'a>($(
            $v: parser_str_to!('a, impl IntoIterator<Item = Token<'a>>),
        )+) -> parser_str_to_iter_token!('a) {
            ($($v),+).map(|($($v),+)| {
                iter::empty() $(.chain($v.into_iter()))+
            })
        }
    }
}

impl_chain!(chain2: a b);
impl_chain!(chain3: a b c);
impl_chain!(chain4: a b c d);

#[cfg(test)]
mod tests {
    use crate::token::{Primitive, Range, Token, TokenStream};
    use combine::Parser;

    macro_rules! tokens {
        ($($t:tt)*) => {{
            let mut result = vec![];
            tokens_impl!(result $($t)*);
            result
        }};
    }
    macro_rules! tokens_impl {
        ($result:ident) => {};
        ($result:ident where $($t:tt)*) => {
            $result.push(Token::Where);
            tokens_impl!($result $($t)*);
        };
        ($result:ident +$ident:ident $($t:tt)*) => {
            $result.push(Token::AssocType(stringify!($ident)));
            tokens_impl!($result $($t)*);
        };
        ($result:ident $ident:ident $($t:tt)*) => {
            $result.push(Token::Identifier(stringify!($ident)));
            tokens_impl!($result $($t)*);
        };
        ($result:ident $str:literal $($t:tt)*) => {
            $result.push(Token::Text($str));
            tokens_impl!($result $($t)*);
        };
        ($result:ident &$r:literal $($t:tt)*) => {
            $result.push(Token::Primitive(Primitive::Ref(concat!("&", $r))));
            tokens_impl!($result $($t)*);
        };
        ($result:ident @() $($t:tt)*) => {
            $result.push(Token::Primitive(Primitive::Unit));
            tokens_impl!($result $($t)*);
        };
        ($result:ident @( $($inner:tt)* ) $($t:tt)*) => {
            $result.push(Token::Primitive(Primitive::TupleStart));
            $result.push(Token::Nested(TokenStream(tokens!($($inner)*))));
            $result.push(Token::Primitive(Primitive::TupleEnd));
            tokens_impl!($result $($t)*);
        };
        ($result:ident @[ $($inner:tt)* ] $($t:tt)*) => {
            $result.push(Token::Primitive(Primitive::SliceStart));
            tokens_impl!($result $($inner)*);
            $result.push(Token::Primitive(Primitive::SliceEnd));
            tokens_impl!($result $($t)*);
        };
        ($result:ident ~$range:ident $($t:tt)*) => {
            $result.push(Token::Range(Range::$range));
            tokens_impl!($result $($t)*);
        };
        ($result:ident @$ident:ident $($t:tt)*) => {
            $result.push(Token::Primitive(Primitive::Named(stringify!($ident))));
            tokens_impl!($result $($t)*);
        };
        ($result:ident { $($inner:tt)* } $($t:tt)*) => {
            $result.push(Token::Nested(TokenStream(tokens!($($inner)*))));
            tokens_impl!($result $($t)*);
        };
    }

    macro_rules! test {
        ($parser:ident: [$($input:literal => [$($expected:tt)*],)*]) => {
            #[test]
            fn $parser() {
                $(
                    let (tokens, remaining) = super::$parser().parse($input)
                        .expect("failed to parse");
                    assert_eq!(remaining, "", "unparsed content");
                    assert_eq!(tokens.collect::<Vec<_>>(), tokens!($($expected)*));
                )*
            }
        };
    }

    test!(item_after_name: [
        " ((T) -> ())" => [" (" { "(" { T } ") -> " @() } ")"],
        " ((&T) -> bool) -> (B, B) where B: Default + Extend<T>" => [
            " (" { "(" { &"" T } ") -> " @bool } ") " "-> " @( B ", " B )
            " " where " " B ": " Default " + " Extend "<" T ">"
        ],
    ]);

    test!(type_like: [
        // Named
        "Foo" => [Foo],
        "Option<Foo>" => [Option "<" Foo ">"],
        "Foo::Err" => [Foo "::" +Err],
        // References
        "&Foo" => [&"" Foo],
        "&'a Foo" => [&"'a" " " Foo],
        "&mut Foo" => [&"mut" " " Foo],
        "&mut 'a Foo" => [&"mut 'a" " " Foo],
        "&[Foo]" => [&"" @[Foo]],
        // Tuple-like
        "()" => [@()],
        "(Foo, &Bar)" => [@(Foo ", " &"" Bar)],
        // Range
        "usize.. usize" => [@usize ~Range " " @usize],
        "usize..=usize" => [@usize ~RangeInclusive @usize],
        "     .. usize" => ["     " ~RangeTo " " @usize],
        "     ..=usize" => ["     " ~RangeToInclusive @usize],
        "usize..      " => [@usize ~RangeFrom "      "],
        "     ..      " => ["     " ~RangeFull "      "],
        // Function
        "() -> Foo" => ["(" ") -> " Foo],
        "(Iterator<Item = T>) -> Result<(), T>" => [
            "(" { Iterator "<" +Item " = " T ">" } ") -> " Result "<" @() ", " T ">"
        ],
        "(Foo, &(Bar, &mut 'a [Baz])) -> T" => [
            "(" { Foo ", " &"" @(Bar ", " &"mut 'a" " " @[Baz]) } ") -> " T
        ],
        // Union (pseudo-type)
        "Foo | &Bar<T> | (Baz) -> bool" => [
            Foo " | " &"" Bar "<" T "> " "| " "(" { Baz } ") -> " @bool
        ],
    ]);
}
