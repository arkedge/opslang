//! Token and keyword definitions
//!
//! This module has three responsibilities:
//! - token definitions via [`declare_token`],
//! - keyword definitions via [`declare_kw`],
//! - public type macro [`V1Token`] from embedded token to type.
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

/// Re-defined version of [`Into`], which does not have `impl<T> Into<T> for T`.
pub trait IntoToken<T>: Sized {
    /// Token lowering.
    ///
    /// Performs conversion between different AST substitutions, typically from AST into IR.
    fn into_token(self) -> T;
}

/// Declares all tokens for v1 grammar.
macro_rules! declare_token {
    ($(pub struct $name:ident/$lit:tt $str:literal)*) => {
        $(
            token_define_if_1!($name/$lit);
            token_define_if_many!($name/$lit);

            impl<'cx, F: super::family::TypeFamily<'cx>> Token for $name<'cx, F> {
                const REPR: &'static str = $str;
            }

            derive_visit!($name);
        )*
    };
}

macro_rules! token_define_if_1 {
    ($name:ident/1) => {
        #[derive(Debug, PartialEq, Clone, Copy)]
        pub struct $name<'cx, F: super::family::TypeFamily<'cx> = super::DefaultTypeFamily> {
            pub position: F::Position,
        }

        impl<'cx, F: super::family::TypeFamily<'cx>, G: super::family::TypeFamily<'cx>>
            IntoToken<$name<'cx, G>> for $name<'cx, F>
        where
            F::Position: IntoPosition<'cx, G>,
        {
            fn into_token(self) -> $name<'cx, G> {
                $name(self.position)
            }
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

        impl<'cx, F: super::family::TypeFamily<'cx>, G: super::family::TypeFamily<'cx>>
            IntoToken<$name<'cx, G>> for $name<'cx, F>
        where
            F::Span: IntoSpan<'cx, G>,
        {
            fn into_token(self) -> $name<'cx, G> {
                $name(self.span)
            }
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

macro_rules! derive_visit {
    ($name:ident) => {
        impl<'cx, F: super::family::TypeFamily<'cx>, V: ?Sized> opslang_visitor::TemplateVisit<V>
            for $name<'cx, F>
        {
        }
        impl<'cx, F: super::family::TypeFamily<'cx>, V: ?Sized> opslang_visitor::TemplateVisitMut<V>
            for $name<'cx, F>
        {
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
    pub struct Star/1 "*"
    pub struct Slash/1 "/"
    pub struct Percent/1 "%"
    pub struct Plus/1 "+"
    pub struct BangEqual/2 "!="
    pub struct SlashEqual/2 "/="
    pub struct EqualEqual/2 "=="
    pub struct RightAngleEq/2 ">="
    pub struct AngleEq/2 "<="
    pub struct ColonEq/2 ":="
    pub struct AndAnd/2 "&&"
    pub struct OrOr/2 "||"
    pub struct Arrow/2 "->"
}

/// Declare all keywords for v1 grammar.
macro_rules! declare_kw {
    ($(pub struct $name:ident $str:literal)*) => {
        $(
            token_define_if_many!($name/2);

            impl<'cx, F: super::family::TypeFamily<'cx>> Token for $name<'cx, F> {
                const REPR: &'static str = $str;
            }

            derive_visit!($name);
        )*
    };
}

declare_kw! {
    pub struct Return "return"
    pub struct Let "let"
    pub struct If "if"
    pub struct Else "else"
    pub struct Prc "prc"
    pub struct Const "const"
    pub struct In "in"
}

#[macro_export]
/// A type-macro that expands to the name of the Rust type representation of a
/// given token.
///
/// Usage is similar to `syn::Token!`.
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
    (prc) => {
        $crate::syntax::v1::token::Prc
    };
    (const) => {
        $crate::syntax::v1::token::Const
    };
    (in) => {
        $crate::syntax::v1::token::In
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
    (*) => {
        $crate::syntax::v1::token::Star
    };
    (/) => {
        $crate::syntax::v1::token::Slash
    };
    (%) => {
        $crate::syntax::v1::token::Percent
    };
    (+) => {
        $crate::syntax::v1::token::Plus
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
    (&&) => {
        $crate::syntax::v1::token::AndAnd
    };
    (||) => {
        $crate::syntax::v1::token::OrOr
    };
    (->) => {
        $crate::syntax::v1::token::Arrow
    };
}
