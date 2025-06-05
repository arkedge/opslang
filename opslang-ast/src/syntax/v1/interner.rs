use crate::{Bridge, derive_trivial_bridge};

macro_rules! declare_interner {
    () => {};
    ($(type $ident:ident;)*) => {
        $(
            type $ident: std::fmt::Debug + PartialEq + Clone + Copy;
        )*
    };
}

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
    declare_interner! {
        type Comment;
        type CommentSpan;
        type ScopeItem;
        type Row;
        type RowContent;
        type Block;
        type ReturnStmt;

        type Ident;
        type Path;

        // Tokens

        type BreakToken;
        type SemiToken;
        type LetToken;
        type EqToken;
        type ColonEqToken;

        // Expression interner

        type Literal;
        type Parened;
        type Qualif;
        type PreQualified;
        type Numeric;
        type Apply;

        type UnOp;
        type CompareOp;
        type BinOp;
    }
}

macro_rules! declare_compat {
    () => {};
    ($(type $ident:ident;)*) => {
        $(
            type $ident: Bridge<Full = <super::DefaultInterner as Interner<'cx>>::$ident>;
        )*
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
