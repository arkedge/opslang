use super::{Position, Span};

pub trait Token {
    /// Source code representation of this token.
    const REPR: &'static str;
}
impl<T: Token> Token for &T {
    const REPR: &'static str = T::REPR;
}

pub trait IntoSpan<'cx, F: super::family::TypeFamily<'cx> = super::DefaultTypeFamily> {
    fn into_span(self) -> F::Span;
}

pub trait IntoPosition<'cx, F: super::family::TypeFamily<'cx> = super::DefaultTypeFamily> {
    fn into_position(self) -> F::Position;
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

        #[doc(hidden)]
        #[inline(always)]
        #[allow(non_snake_case)]
        pub fn $name<'cx, F: super::family::TypeFamily<'cx>>(
            position: impl IntoPosition<'cx, F>,
        ) -> $name<'cx, F> {
            $name {
                position: position.into_position(),
            }
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

        #[doc(hidden)]
        #[inline(always)]
        #[allow(non_snake_case)]
        pub fn $name<'cx, F: super::family::TypeFamily<'cx>>(
            span: impl IntoSpan<'cx, F>,
        ) -> $name<'cx, F> {
            $name {
                span: span.into_span(),
            }
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
    pub struct Dollar/1 "$"
    pub struct Question/1 "?"
    pub struct RightAngle/1 ">"
    pub struct Angle/1 "<"
    pub struct BangEqual/2 "!="
    pub struct SlashEqual/2 "/="
    pub struct EqualEqual/2 "=="
    pub struct RightAngleEq/2 ">="
    pub struct AngleEq/2 "<="
    pub struct ColonEq/2 ":="
}

macro_rules! declare_kw {
    ($(pub struct $name:ident $str:literal)*) => {
        $(
            token_define_if_many!($name/2);

            impl<'cx, F: super::family::TypeFamily<'cx>> Token for $name<'cx, F> {
                const REPR: &'static str = $str;
            }
        )*
    };
}

declare_kw! {
    pub struct Return "return"
    pub struct Let "let"
    pub struct If "if"
    pub struct Else "else"
    pub struct Proc "proc"
    pub struct Const "const"
}

#[macro_export]
/// A type-macro that expands to the name of the Rust type representation of a
/// given token.
macro_rules! V1Token {
    // Keywords
    (return) => {
        $crate::syntax::v1::token::Return
    };
    (let) => {
        $crate::syntax::v1::token::Let
    };
    (if) => {
        $crate::syntax::v1::token::If
    };
    (else) => {
        $crate::syntax::v1::token::Else
    };
    (proc) => {
        $crate::syntax::v1::token::Proc
    };
    (const) => {
        $crate::syntax::v1::token::Const
    };
    
    // Single character tokens
    (;) => {
        $crate::syntax::v1::token::Semi
    };
    (.) => {
        $crate::syntax::v1::token::Break
    };
    (@) => {
        $crate::syntax::v1::token::Atmark
    };
    (~) => {
        $crate::syntax::v1::token::Tilde
    };
    (:) => {
        $crate::syntax::v1::token::Colon
    };
    (=) => {
        $crate::syntax::v1::token::Eq
    };
    // Note: {, }, (, ), [, ] cannot be defined in Rust macros due to syntax limitations
    (-) => {
        $crate::syntax::v1::token::Hyphen
    };
    (&) => {
        $crate::syntax::v1::token::Ampersand
    };
    ($) => {
        $crate::syntax::v1::token::Dollar
    };
    (?) => {
        $crate::syntax::v1::token::Question
    };
    (>) => {
        $crate::syntax::v1::token::RightAngle
    };
    (<) => {
        $crate::syntax::v1::token::Angle
    };
    
    // Two character tokens
    (!=) => {
        $crate::syntax::v1::token::BangEqual
    };
    (/=) => {
        $crate::syntax::v1::token::SlashEqual
    };
    (==) => {
        $crate::syntax::v1::token::EqualEqual
    };
    (>=) => {
        $crate::syntax::v1::token::RightAngleEq
    };
    (<=) => {
        $crate::syntax::v1::token::AngleEq
    };
    (:=) => {
        $crate::syntax::v1::token::ColonEq
    };
}
