use std::fmt::Debug;

/// Shorthand for repeating trait constraints.
macro_rules! declare_family {
    ($(type $ident:ident;)*) => {
        $(
            type $ident: std::fmt::Debug + PartialEq + Clone + Copy;
        )*
    };
}

/// Type family that occurs in almost every types to allow global type substitution.
/// It is known as "trees that grow".
///
/// # Operation in this project
///
/// This trait takes a lifetime parameter `'cx` so that the members can refer to it
/// regardless of whether they use it or not, and the members can have default
/// type parameters.
///
/// This trait carries all of possible substitution, including recursive elements.
/// If you find yourself creating definition similar to types in this crate,
/// try to avoid copying types by adding members of this trait.
///
/// Modifying this will require changes of the following elements:
/// - [`v1_default_type_subst!`] macro (for [`DefaultTypeFamily`]),
/// - `TypeFamily` type in `opslang-ir` crate,
/// - `v0_to_v1` module in `opslang-migration` crate.
///
/// [`v1_default_type_subst!`]: crate::v1_default_type_subst
/// [`DefaultTypeFamily`]: crate::v1::DefaultTypeFamily
pub trait TypeFamily<'cx>: Debug + PartialEq + Clone + Copy + Default + 'static {
    declare_family! {
        type Span;
        type Position;

        type Comment;
        type ScopeItem;
        type Row;
        type RowContent;
        type Block;
        type ReturnStmt;

        type Ident;
        type Path;

        // Expression types
        type Expr;

        type Literal;
        type Parened;
        type Qualif;
        type PreQualified;
        type Numeric;
        type Apply;
    }
}
