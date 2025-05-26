use crate::{Bridge, derive_trivial_bridge};

/// Type family that interns almost every types, to allow global type substitution.
/// It is known as "trees that grow".
///
/// # Operation in this crate
///
/// This trait takes a lifetime parameter `'cx` so that the members can refer to it
/// regardless of whether they use it or not, and the members can have default
/// type parameters.
///
/// This trait carries all of possible substitution, including recursive elements.
pub trait Interner<'cx> {
    type Comment: InternerMember;
    type CommentSpan: InternerMember;
    type ScopeItem: InternerMember;
    type Row: InternerMember;
    type RowContent: InternerMember;
    type Block: InternerMember;
    type ReturnStmt: InternerMember;

    type Ident: InternerMember;
    type Path: InternerMember;

    // Tokens

    type BreakToken: InternerMember;
    type SemiToken: InternerMember;
    type LetToken: InternerMember;
    type EqToken: InternerMember;
    type ColonEqToken: InternerMember;

    // Expression interner

    type Literal: ExprInternerMember;
    type Parened: ExprInternerMember;
    type Qualif: ExprInternerMember;
    type PreQualified: ExprInternerMember;
    type Numeric: ExprInternerMember;
    type Apply: ExprInternerMember;

    type UnOp: ExprInternerMember;
    type CompareOp: ExprInternerMember;
    type BinOp: ExprInternerMember;
}

pub trait InternerMember = std::fmt::Debug + PartialEq + Clone + Copy;
pub trait ExprInternerMember = std::fmt::Debug + PartialEq + Clone + Copy;

macro_rules! declare_compat {
    () => {};
    (type $ident:ident; $($rest:tt)*) => {
        type $ident: Bridge<Full = <super::DefaultInterner as Interner<'cx>>::$ident>;
        declare_compat!{ $($rest)* }
    };
}

/// Part of interner that is different between compatible versions.
///
/// This is used to convert between different versions of the AST.
pub trait CompatV0<'cx> {
    declare_compat! {
        type BreakToken;
        type Comment;
        type CommentSpan;
        type RowContent;
    }
}

derive_trivial_bridge!([for 'cx] &'cx super::Comment<'cx>);
