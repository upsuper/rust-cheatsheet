use crate::token::{Primitive, Range, Token, TokenStream};
use combine::error::StringStreamError;
use combine::parser::{
    char::{alpha_num, char, letter, spaces, string},
    choice::{choice, optional},
    combinator::attempt,
    range::recognize,
    repeat::{many, skip_many1},
    Parser,
};
use either_n::{Either2, Either3, Either6};
use std::iter;

pub struct ParsedItem<'a> {
    pub takes_self: bool,
    pub name: &'a str,
    pub tokens: TokenStream<'a>,
}

impl<'a> ParsedItem<'a> {
    pub fn parse(input: &'a str) -> Result<Self, ()> {
        (optional(string("::")), identifier_str(), item_after_name())
            .parse(input)
            .map_err(|_| ())
            .and_then(|((prefix, name, rest), remaining)| match remaining {
                "" => Ok(ParsedItem {
                    takes_self: prefix.is_none(),
                    name,
                    tokens: TokenStream(rest.collect()),
                }),
                _ => Err(()),
            })
    }
}

pub fn parse_type(input: &str) -> Result<TokenStream<'_>, ()> {
    single_type_like_token()
        .parse(input)
        .map_err(|_| ())
        .and_then(|(token, remaining)| {
            let token_stream = match token {
                Token::Type(inner) => inner,
                _ => unreachable!(),
            };
            match remaining {
                "" => Ok(token_stream),
                _ => Err(()),
            }
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
    chain5(
        lex("("),
        nested_type_like_list(),
        lex(")"),
        optional_tokens(chain2(lex("->"), single_type_like())),
        optional_tokens(chain2(wrap("where", Token::Where), where_constraints())),
    )
}

fn where_constraints<'a>() -> parser_str_to_iter_token!('a) {
    sep1_by_lex(single_where_constraint, ",")
}

fn single_where_constraint<'a>() -> parser_str_to_iter_token!('a) {
    chain3(
        single_type_like(),
        lex(":"),
        sep1_by_lex(single_type_like, "+"),
    )
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

// Add an extra wrapper for this parser so that we don't have too deep type name.
parser! {
    fn single_type_like['a]()(&'a str) -> BoxedTokenIter<'a> {
        single_type_like_inner()
    }
}

fn single_type_like_inner<'a>() -> parser_str_to!('a, BoxedTokenIter<'a>) {
    single_type_like_token().map(iter::once).map(to_boxed_iter)
}

fn to_boxed_iter<'a, T>(iter: impl Iterator<Item = T> + 'a) -> Box<dyn Iterator<Item = T> + 'a> {
    Box::new(iter)
}

fn single_type_like_token<'a>() -> parser_str_to!('a, Token<'a>) {
    to_type_token(choice((
        attempt(ref_type()).map(Either6::One),
        attempt(slice_type()).map(Either6::Two),
        attempt(fn_type()).map(Either6::Three),
        attempt(tuple_type()).map(Either6::Four),
        attempt(range_type()).map(Either6::Five),
        named_type().map(Either6::Six),
    )))
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
        single_type_like(),
    )
}

fn slice_type<'a>() -> parser_str_to_iter_token!('a) {
    chain3(
        wrap_start("[", Primitive::SliceStart),
        type_like(),
        wrap_end("]", Primitive::SliceEnd),
    )
}

fn fn_type<'a>() -> parser_str_to_iter_token!('a) {
    chain4(
        text((char('('), spaces())),
        nested_type_like_list(),
        text((spaces(), char(')'), spaces(), string("->"), spaces())),
        type_like(),
    )
}

fn tuple_type<'a>() -> parser_str_to_iter_token!('a) {
    choice((
        attempt(wrap("()", Primitive::Unit)).map(Either2::One),
        chain3(
            wrap_start("(", Primitive::TupleStart),
            nested_type_like_list(),
            wrap_end(")", Primitive::TupleEnd),
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

fn range_type<'a>() -> parser_str_to_iter_token!('a) {
    (
        optional(named_type()),
        choice((attempt(lex_str("..=")), attempt(lex_str("..")))),
        optional(named_type()),
    )
        .and_then(|(start, op, end)| {
            let range = match (&start, op.trim(), &end) {
                (None, "..", None) => Range::RangeFull,
                (None, "..", Some(_)) => Range::RangeTo,
                (None, "..=", Some(_)) => Range::RangeToInclusive,
                (Some(_), "..", None) => Range::RangeFrom,
                (Some(_), "..", Some(_)) => Range::Range,
                (Some(_), "..=", Some(_)) => Range::RangeInclusive,
                _ => return Err(StringStreamError::UnexpectedParse),
            };
            let start = start.into_iter().flatten();
            let end = end.into_iter().flatten();
            Ok(iter::empty()
                .chain(start)
                .chain(range_token(op, range))
                .chain(end))
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
    chain2(
        named_type_base().map(|ty| iter::once(Token::Type(ty.collect()))),
        // Associated items
        many::<TokenStream<'_>, _>(attempt(chain2(
            lex("::"),
            identifier_str().map(Token::AssocType).map(iter::once),
        ))),
    )
}

fn named_type_base<'a>() -> parser_str_to_iter_token!('a) {
    chain2(
        // Name
        identifier_str().map(|ident| {
            iter::once(if is_primitive(ident) {
                Token::Primitive(Primitive::Named(ident))
            } else {
                Token::Identifier(ident)
            })
        }),
        // Optional parameters
        optional_tokens(chain3(
            lex("<"),
            sep1_by_lex(type_param, ","),
            text((spaces(), char('>'))),
        )),
    )
}

fn to_type_token<'a>(inner: parser_str_to_iter_token!('a)) -> parser_str_to!('a, Token<'a>) {
    inner.map(|ty| {
        let mut inner: Vec<_> = ty.collect();
        match inner.as_ref() as &[_] {
            [Token::Type(_)] => inner.remove(0),
            _ => Token::Type(TokenStream(inner)),
        }
    })
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

fn wrap_start<'a>(
    inner: &'static str,
    token: impl Into<Token<'a>>,
) -> parser_str_to_iter_token!('a) {
    let token = token.into();
    chain2(
        string(inner).map(move |_| iter::once(token.clone())),
        maybe_spaces(),
    )
}

fn wrap_end<'a>(inner: &'static str, token: impl Into<Token<'a>>) -> parser_str_to_iter_token!('a) {
    let token = token.into();
    chain2(
        maybe_spaces(),
        string(inner).map(move |_| iter::once(token.clone())),
    )
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
impl_chain!(chain5: a b c d e);

#[cfg(test)]
mod tests {
    use combine::Parser;
    use pretty_assertions::assert_eq;

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
        " ((T) -> ())" => [" (" { ^["(" { ^T } ") -> " @()] } ")"],
        " ((&T) -> bool) -> (B, B) where B: Default + Extend<T>" => [
            " (" { ^["(" { ^[&"" ^T] } ") -> " @bool] } ") " "-> " @( ^B ", " ^B )
            " " where " " ^B ": " ^Default " + " ^[ Extend "<" ^T ">" ]
        ],
        " (S, T) -> S where S: Default + Clone, Tz::Offset: Display" => [
            " (" { ^S ", " ^T } ") " "-> " ^S " " where " "
            ^S ": " ^Default " + " ^Clone ", " ^[ ^Tz "::" +Offset ] ": " ^Display
        ],
    ]);

    test!(type_like: [
        // Named
        "Foo" => [^Foo],
        "Option<Foo>" => [^[Option "<" ^Foo ">"]],
        "Foo::Err" => [^[^Foo "::" +Err]],
        // References
        "&Foo" => [^[&"" ^Foo]],
        "&'a Foo" => [^[&"'a" " " ^Foo]],
        "&mut Foo" => [^[&"mut" " " ^Foo]],
        "&mut 'a Foo" => [^[&"mut 'a" " " ^Foo]],
        "&[Foo]" => [^[&"" @[^Foo]]],
        // Tuple-like
        "()" => [@()],
        "(Foo, &Bar)" => [@(^Foo ", " ^[&"" ^Bar])],
        // Range
        "usize.. usize" => [^[@usize ~Range " " @usize]],
        "usize..=usize" => [^[@usize ~RangeInclusive @usize]],
        "     .. usize" => [^["     " ~RangeTo " " @usize]],
        "     ..=usize" => [^["     " ~RangeToInclusive @usize]],
        "usize..      " => [^[@usize ~RangeFrom "      "]],
        "     ..      " => [^["     " ~RangeFull "      "]],
        // Function
        "() -> Foo" => [^["(" ") -> " ^Foo]],
        "(Iterator<Item = T>) -> Result<(), T>" => [
            ^["(" { ^[Iterator "<" +Item " = " ^T ">"] } ") -> " ^[Result "<" @() ", " ^T ">"]]
        ],
        "(Foo, &(Bar, &mut 'a [Baz])) -> T" => [
            ^["(" { ^Foo ", " ^[&"" @(^Bar ", " ^[&"mut 'a" " " @[^Baz]])] } ") -> " ^T]
        ],
        // Union (pseudo-type)
        "Foo | &Bar<T> | (Baz) -> bool" => [
            ^Foo " | " ^[&"" ^[Bar "<" ^T ">"]] " | " ^["(" { ^Baz } ") -> " @bool]
        ],
    ]);
}
