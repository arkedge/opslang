use opslang_ast_macros::{Position, Span};

use super::{Position, Span};

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
    ($(pub struct $name:ident/$lit:tt)*) => {
        $(
            token_define_if_1!($name/$lit);
            token_define_if_many!($name/$lit);
        )*
    };
}

macro_rules! token_define_if_1 {
    ($name:ident/1) => {
        #[derive(Debug, PartialEq, Clone, Copy, Position)]
        pub struct $name {
            pub position: Position,
        }
    };
    ($name:ident/$lit:tt) => {};
}

macro_rules! token_define_if_many {
    ($name:ident/1) => {};
    ($name:ident/$lit:tt) => {
        #[derive(Debug, PartialEq, Clone, Copy, Span)]
        pub struct $name {
            pub span: Span,
        }
    };
}

declare_token! {
    pub struct Semi/1
    pub struct Break/1
    pub struct Atmark/1
    pub struct Eq/1
    pub struct OpenBrace/1
    pub struct CloseBrace/1
    pub struct OpenParen/1
    pub struct CloseParen/1
    pub struct OpenSquare/1
    pub struct CloseSquare/1
    pub struct Hyphen/1
    pub struct Ampersand/1
    pub struct RightAngle/1
    pub struct Angle/1
    pub struct BangEqual/2
    pub struct SlashEqual/2
    pub struct EqualEqual/2
    pub struct RightAngleEq/2
    pub struct AngleEq/2
    pub struct ColonEq/2
}

#[derive(Debug, PartialEq, Clone, Copy, Span)]
pub struct Return {
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy, Span)]
pub struct Let {
    pub span: Span,
}
