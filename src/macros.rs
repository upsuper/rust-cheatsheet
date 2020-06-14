#[cfg(test)]
#[macro_export]
macro_rules! tokens {
    ($($t:tt)*) => {{
        #[allow(unused_imports)]
        use crate::token::{Primitive, Range, Token, TokenStream};
        let mut result = vec![];
        tokens_impl!(result $($t)*);
        result
    }};
}

#[cfg(test)]
#[macro_export]
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
    ($result:ident *$r:literal $($t:tt)*) => {
        $result.push(Token::Primitive(Primitive::Ptr(concat!("*", $r))));
        tokens_impl!($result $($t)*);
    };
    ($result:ident @() $($t:tt)*) => {
        $result.push(Token::Type(TokenStream(vec![
            Token::Primitive(Primitive::Unit),
        ])));
        tokens_impl!($result $($t)*);
    };
    ($result:ident @( $($inner:tt)* ) $($t:tt)*) => {
        $result.push(Token::Type(TokenStream(vec![
            Token::Primitive(Primitive::TupleStart),
            Token::Nested(TokenStream(tokens!($($inner)*))),
            Token::Primitive(Primitive::TupleEnd),
        ])));
        tokens_impl!($result $($t)*);
    };
    ($result:ident @[ $($inner:tt)* ] $($t:tt)*) => {
        let mut inner = vec![];
        inner.push(Token::Primitive(Primitive::SliceStart));
        tokens_impl!(inner $($inner)*);
        inner.push(Token::Primitive(Primitive::SliceEnd));
        $result.push(Token::Type(TokenStream(inner)));
        tokens_impl!($result $($t)*);
    };
    ($result:ident ~$range:ident $($t:tt)*) => {
        $result.push(Token::Range(Range::$range));
        tokens_impl!($result $($t)*);
    };
    ($result:ident @$ident:ident $($t:tt)*) => {
        $result.push(Token::Type(TokenStream(vec![
            Token::Primitive(Primitive::Named(stringify!($ident))),
        ])));
        tokens_impl!($result $($t)*);
    };
    ($result:ident ^$ident:ident $($t:tt)*) => {
        $result.push(Token::Type(TokenStream(vec![
            Token::Identifier(stringify!($ident)),
        ])));
        tokens_impl!($result $($t)*);
    };
    ($result:ident ^[ $($inner:tt)* ] $($t:tt)*) => {
        $result.push(Token::Type(TokenStream(tokens!($($inner)*))));
        tokens_impl!($result $($t)*);
    };
    ($result:ident { $($inner:tt)* } $($t:tt)*) => {
        $result.push(Token::Nested(TokenStream(tokens!($($inner)*))));
        tokens_impl!($result $($t)*);
    };
}
