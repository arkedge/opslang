use super::{Position, Span};

pub trait Token {
    /// Source code representation of this token.
    const REPR: &'static str;
}
impl<T: Token> Token for &T {
    const REPR: &'static str = T::REPR;
}

#[macro_export]
/// A type-macro that expands to the name of the Rust type representation of a
/// given token.
macro_rules! V1Token {
    (return) => {
        $crate::syntax::v1::token::Return
    };
    (;) => {
        $crate::syntax::v1::token::Semi
    };
    (.) => {
        $crate::syntax::v1::token::Break
    };
    (@) => {
        $crate::syntax::v1::token::Atmark
    };
    (let) => {
        $crate::syntax::v1::token::Let
    };
    (=) => {
        $crate::syntax::v1::token::Eq
    };
    (<-) => {
        $crate::syntax::v1::token::AngleHyphen
    };
    (<=) => {
        $crate::syntax::v1::token::RightAngleEq
    };
    (<=) => {
        $crate::syntax::v1::token::AngleEq
    };
    (>) => {
        $crate::syntax::v1::token::RightAngle
    };
    (<) => {
        $crate::syntax::v1::token::Angle
    };
    (!=) => {
        $crate::syntax::v1::token::BangEqual
    };
    (/=) => {
        $crate::syntax::v1::token::SlashEqual
    };
    (==) => {
        $crate::syntax::v1::token::EqualEqual
    };
}

macro_rules! declare_token {
    ($(pub struct $name:ident/$lit:tt $str:literal)*) => {
        $(
            token_define_if_1!($name/$lit);
            token_define_if_many!($name/$lit);

            impl<'cx, F: super::family::TypeFamily<'cx>> Token for $name<'cx, F> {
                const REPR: &'static str = $str;
            }
        )*
    };
}

macro_rules! token_define_if_1 {
    ($name:ident/1) => {
        #[derive(Debug, PartialEq, Clone, Copy)]
        pub struct $name<'cx, F: super::family::TypeFamily<'cx> = super::DefaultTypeFamily> {
            pub position: F::Position,
        }
        impl<'cx, F: super::family::TypeFamily<'cx, Position: super::loc::Position>>
            super::loc::Position for $name<'cx, F>
        {
            fn position(&self) -> Position {
                self.position.position()
            }
        }
    };
    ($name:ident/$lit:tt) => {};
}

macro_rules! token_define_if_many {
    ($name:ident/1) => {};
    ($name:ident/$lit:tt) => {
        #[derive(Debug, PartialEq, Clone, Copy)]
        pub struct $name<'cx, F: super::family::TypeFamily<'cx> = super::DefaultTypeFamily> {
            pub span: F::Span,
        }
        impl<'cx, F: super::family::TypeFamily<'cx, Span: super::loc::Span>> super::loc::Span
            for $name<'cx, F>
        {
            fn span(&self) -> Span {
                self.span.span()
            }
        }
    };
}

declare_token! {
    pub struct Semi/1 ";"
    pub struct Break/1 "."
    pub struct Atmark/1 "@"
    pub struct Tilde/1 "~"
    pub struct Colon/1 ":"
    pub struct Eq/1 "="
    pub struct OpenBrace/1 "{"
    pub struct CloseBrace/1 "}"
    pub struct OpenParen/1 "("
    pub struct CloseParen/1 ")"
    pub struct OpenSquare/1 "["
    pub struct CloseSquare/1 "]"
    pub struct Hyphen/1 "-"
    pub struct Ampersand/1 "&"
    pub struct RightAngle/1 ">"
    pub struct Angle/1 "<"
    pub struct BangEqual/2 "!="
    pub struct SlashEqual/2 "/="
    pub struct EqualEqual/2 "=="
    pub struct RightAngleEq/2 ">="
    pub struct AngleEq/2 "<="
    pub struct ColonEq/2 ":="

    pub struct Return/6 "return"
    pub struct Let/3 "let"
}
