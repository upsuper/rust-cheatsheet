use crate::token::{Primitive, Token, TokenStream};
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
macro_rules! parser_iter_token {
    ($a:lifetime) => {
        impl Parser<Input = &$a str, Output = impl Iterator<Item = Token<$a>>>
    }
}

fn item_after_name<'a>() -> parser_iter_token!('a) {
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

fn type_like_inner<'a>() -> impl Parser<Input = &'a str, Output = BoxedTokenIter<'a>> {
    sep1_by_lex(single_type_like, "|").map(to_boxed_iter)
}

fn to_boxed_iter<'a, T>(iter: impl Iterator<Item = T> + 'a) -> Box<dyn Iterator<Item = T> + 'a> {
    Box::new(iter)
}

fn single_type_like<'a>() -> parser_iter_token!('a) {
    choice((
        attempt(ref_mut_type()).map(Either6::One),
        attempt(ref_type()).map(Either6::Two),
        attempt(slice_type()).map(Either6::Three),
        attempt(fn_type()).map(Either6::Four),
        attempt(tuple_type()).map(Either6::Five),
        named_type().map(Either6::Six),
    ))
}

fn ref_mut_type<'a>() -> parser_iter_token!('a) {
    chain3(
        recognize((string("&mut"), optional(attempt((spaces(), lifetime())))))
            .map(|s| iter::once(Token::Primitive(Primitive::Ref(s)))),
        maybe_spaces(),
        type_like(),
    )
}

fn ref_type<'a>() -> parser_iter_token!('a) {
    chain3(
        recognize((char('&'), optional(lifetime())))
            .map(|s| iter::once(Token::Primitive(Primitive::Ref(s)))),
        maybe_spaces(),
        type_like(),
    )
}

fn slice_type<'a>() -> parser_iter_token!('a) {
    chain3(
        wrap("[", Primitive::SliceStart),
        type_like(),
        wrap("]", Primitive::SliceEnd),
    )
}

fn fn_type<'a>() -> parser_iter_token!('a) {
    chain4(
        lex("("),
        nested_type_like_list(),
        text((spaces(), char(')'), spaces(), string("->"), spaces())),
        type_like(),
    )
}

fn tuple_type<'a>() -> parser_iter_token!('a) {
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

fn nested_type_like_list<'a>() -> parser_iter_token!('a) {
    optional(
        sep1_by_lex(type_like, ",")
            .map(Iterator::collect)
            .map(Token::Nested),
    )
    .map(IntoIterator::into_iter)
}

fn named_type<'a>() -> parser_iter_token!('a) {
    chain3(
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

fn type_param<'a>() -> parser_iter_token!('a) {
    choice((
        attempt(lifetime_param()).map(Either3::One),
        attempt(assoc_type_param()).map(Either3::Two),
        type_like().map(Either3::Three),
    ))
}

fn lifetime_param<'a>() -> parser_iter_token!('a) {
    text(lifetime())
}

fn assoc_type_param<'a>() -> parser_iter_token!('a) {
    chain3(identifier(), lex("="), type_like())
}

fn optional_tokens<'a>(inner: parser_iter_token!('a)) -> parser_iter_token!('a) {
    optional(attempt(inner))
        .map(IntoIterator::into_iter)
        .map(Iterator::flatten)
}

fn sep1_by_lex<'a, P, I>(parser_fn: impl Fn() -> P, sep: &'static str) -> parser_iter_token!('a)
where
    P: Parser<Input = &'a str, Output = I>,
    I: Iterator<Item = Token<'a>>,
{
    chain2(
        parser_fn(),
        many::<TokenStream<'a>, _>(attempt(chain2(lex(sep), parser_fn()))),
    )
}

fn lex<'a>(s: &'static str) -> parser_iter_token!('a) {
    text((spaces(), string(s), spaces()))
}

fn wrap<'a>(inner: &'static str, token: impl Into<Token<'a>>) -> parser_iter_token!('a) {
    let token = token.into();
    chain3(
        maybe_spaces(),
        string(inner).map(move |_| iter::once(token.clone())),
        maybe_spaces(),
    )
}

fn maybe_spaces<'a>() -> parser_iter_token!('a) {
    recognize(spaces()).map(|s| match s {
        "" => None.into_iter(),
        s => Some(Token::Text(s)).into_iter(),
    })
}

fn text<'a>(inner: impl Parser<Input = &'a str>) -> parser_iter_token!('a) {
    text_token(inner).map(iter::once)
}

fn text_token<'a>(
    inner: impl Parser<Input = &'a str>,
) -> impl Parser<Input = &'a str, Output = Token<'a>> {
    recognize(inner).map(Token::Text)
}

fn lifetime<'a>() -> impl Parser<Input = &'a str, Output = &'a str> {
    recognize((char('\''), skip_many1(letter())))
}

fn identifier<'a>() -> parser_iter_token!('a) {
    identifier_str().map(Token::Identifier).map(iter::once)
}

fn identifier_str<'a>() -> impl Parser<Input = &'a str, Output = &'a str> {
    recognize(skip_many1(choice((alpha_num(), char('_')))))
}

macro_rules! impl_chain {
    ($name:ident: $($v:ident)+) => {
        fn $name<'a>($(
            $v: impl Parser<Input = &'a str, Output = impl IntoIterator<Item = Token<'a>>>,
        )+) -> parser_iter_token!('a) {
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
    use crate::parser::*;
    use crate::token::{Primitive, Token, TokenStream};
    use combine::Parser;

    fn assert_parse<'a, P, I>(mut parser: P, input: &'a str) -> Vec<Token<'a>>
    where
        I: Iterator<Item = Token<'a>>,
        P: Parser<Input = &'a str, Output = I>,
    {
        let (tokens, remaining) = parser.parse(input).expect("failed to parse");
        assert_eq!(remaining, "", "unparsed content");
        tokens.collect()
    }

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
        ($result:ident @$ident:ident $($t:tt)*) => {
            $result.push(Token::Primitive(Primitive::Named(stringify!($ident))));
            tokens_impl!($result $($t)*);
        };
        ($result:ident { $($inner:tt)* } $($t:tt)*) => {
            $result.push(Token::Nested(TokenStream(tokens!($($inner)*))));
            tokens_impl!($result $($t)*);
        };
    }

    #[test]
    fn test_item_after_name() {
        fn parse(s: &str) -> Vec<Token<'_>> {
            assert_parse(item_after_name(), s)
        }
        assert_eq!(
            parse(" ((T) -> ())"),
            tokens!(" (" { "(" { T } ") -> " @() } ")"),
        );
        assert_eq!(
            parse(" ((&T) -> bool) -> (B, B) where B: Default + Extend<T>"),
            tokens!(
                " (" { "(" { &"" T } ") -> " @bool } ") " "-> " @( B ", " B )
                " " where " " B ": " Default " + " Extend "<" T ">"
            ),
        );
    }

    #[test]
    fn test_type_like() {
        fn parse(s: &str) -> Vec<Token<'_>> {
            assert_parse(type_like(), s)
        }
        assert_eq!(parse("Foo"), tokens!(Foo));
        assert_eq!(parse("Option<Foo>"), tokens!(Option "<" Foo ">"));
        assert_eq!(parse("&Foo"), tokens!(&"" Foo));
        assert_eq!(parse("&'a Foo"), tokens!(&"'a" " " Foo));
        assert_eq!(parse("&mut Foo"), tokens!(&"mut" " " Foo));
        assert_eq!(parse("&mut 'a Foo"), tokens!(&"mut 'a" " " Foo));
        assert_eq!(parse("&[Foo]"), tokens!(&"" @[Foo]));
        assert_eq!(parse("()"), tokens!(@()));
        assert_eq!(parse("(Foo, &Bar)"), tokens!(@(Foo ", " &"" Bar)));
        assert_eq!(parse("() -> Foo"), tokens!("(" ") -> " Foo));
        assert_eq!(
            parse("(Iterator<Item = T>) -> Result<(), T>"),
            tokens!("(" { Iterator "<" Item " = " T ">" } ") -> " Result "<" @() ", " T ">"),
        );
        assert_eq!(
            parse("(Foo, &(Bar, &mut 'a [Baz])) -> T"),
            tokens!("(" { Foo ", " &"" @(Bar ", " &"mut 'a" " " @[Baz]) } ") -> " T),
        );
        assert_eq!(
            parse("Foo | &Bar<T> | (Baz) -> bool"),
            tokens!(Foo " | " &"" Bar "<" T "> " "| " "(" { Baz } ") -> " @bool),
        );
    }
}
