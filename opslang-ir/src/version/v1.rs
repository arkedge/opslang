/*!
Intermediate Representation (IR) for opslang v1.

The IR serves as a bridge between the Abstract Syntax Tree (AST) and the type-checked representation.
This module defines the responsibility and structure of the IR layer.

## IR Responsibilities

The IR has several key responsibilities that distinguish it from the AST:

1. **Type Inference Results**: The IR holds the results of type inference, including resolved
   types for expressions, identifiers, and literals. This is primarily achieved through the
   `v1_default_type_subst!` macro which configures `Expr` to be typed.

2. **Name Resolution Results**: AST `Ident` types are resolved to `opslang_ty::Ident` with scope
   information, and AST `Path` types are resolved to `opslang_ty::ModuleItem` references
   after module resolution.

3. **Qualification Integration**: Qualif and PreQualified expressions are integrated into Apply
   expressions, and these qualification types are made infallible (using `std::convert::Infallible`)
   to prevent construction in the IR, as they should be resolved into Apply operations.

4. **Parsed Literal Values**: Literals are stored as their parsed values rather than raw strings.
   While the AST uses permissive parsing to avoid early failures, the IR contains only the
   meaningful parsed values since validation has already occurred.

5. **Comment Merging**: Adjacent comments that were stored line-by-line in the AST are merged
   into comment blocks in the IR. This reflects the common practice of multi-line comments
   and provides a more convenient representation for downstream processing.

The IR represents the result of type inference and name resolution, providing a more semantic
view of the program structure while maintaining references to the original AST for source
location information and tooling support.
*/

use chrono::Utc;
use opslang_ast::{
    v1::{self as syn, TypeFamily as AstTypeFamily},
    v1_default_type_subst,
};
use opslang_ty::version::v1::{Ident, Ty};
use std::convert::Infallible;

#[derive(Debug, PartialEq, Clone, Copy, Default)]
/// IR type family that includes type information and resolved names.
pub struct IrTypeFamily;

#[derive(Debug, Clone, Copy)]
/// A resolved path reference to a definition or module item.
pub struct ResolvedPath<'cx> {
    /// Reference to the resolved module item.
    pub item: &'cx opslang_ty::version::v1::ModuleItem<'cx>,
    /// The original path that was resolved.
    pub original_path: &'cx syn::Path<'cx>,
}

impl<'cx> PartialEq for ResolvedPath<'cx> {
    fn eq(&self, other: &Self) -> bool {
        // Compare by pointer address since ModuleItems should be unique
        std::ptr::eq(self.item, other.item) && self.original_path == other.original_path
    }
}

impl<'cx> AstTypeFamily<'cx> for IrTypeFamily {
    v1_default_type_subst! {
        Span = Option<syn::Span>,
        Position = Option<syn::Position>,

        // Core structural types
        Comment = Comment<'cx>,

        // Resolved name types
        Ident = Ident<'cx>,
        Path = ResolvedPath<'cx>,

        // Typed expression
        Expr = Expr<'cx>,

        // Infallible qualification types (resolved into Apply)
        Qualif = Infallible,
        PreQualified = Infallible,

        // Expression components
        String = String<'cx>,
        Bytes = Bytes<'cx>,
        HexBytes = HexBytes<'cx>,
        DateTime = DateTime<'cx>,
        Numeric = Numeric<'cx>,
        Apply = Apply<'cx>,
        ..
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A merged comment block in the IR.
///
/// Unlike AST comments which are stored line-by-line, IR comments represent
/// merged blocks of adjacent comments, which is more natural for multi-line
/// comment blocks in the source code.
pub struct Comment<'cx> {
    /// The combined content of all adjacent comments in the block.
    pub content: &'cx str,
    /// The span covering all merged comments.
    pub span: syn::Span,
    /// References to the original AST comments that were merged.
    pub source_comments: &'cx [&'cx syn::Comment<'cx>],
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A typed expression in the IR.
///
/// This is the core difference from AST expressions - IR expressions carry
/// type information from the type inference process.
pub struct Expr<'cx> {
    /// The expression kind/content.
    pub kind: &'cx syn::ExprKind<'cx, IrTypeFamily>,
    /// The inferred type of this expression.
    pub ty: Ty<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A parsed string literal in the IR.
pub struct String<'cx> {
    /// The parsed string value (escape sequences processed).
    pub value: &'cx str,
    /// Reference to the original AST string.
    pub syn: &'cx syn::literal::String<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A parsed bytes literal in the IR.
pub struct Bytes<'cx> {
    /// The parsed byte array.
    pub value: &'cx [u8],
    /// Reference to the original AST bytes literal.
    pub syn: &'cx syn::literal::Bytes<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A parsed hexadecimal bytes literal in the IR.
pub struct HexBytes<'cx> {
    /// The parsed byte array from hex representation.
    pub value: &'cx [u8],
    /// Reference to the original AST hex bytes literal.
    pub syn: &'cx syn::literal::HexBytes<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A parsed datetime literal in the IR.
pub struct DateTime<'cx> {
    /// The parsed datetime value.
    pub value: chrono::DateTime<Utc>,
    /// Reference to the original AST datetime literal.
    pub syn: &'cx syn::literal::DateTime<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A parsed numeric literal in the IR.
pub struct Numeric<'cx> {
    /// The value.
    pub kind: NumericKind,
    /// Reference to the original AST numeric literal.
    pub syn: &'cx syn::literal::Numeric<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A parsed numeric literal in the IR.
pub enum NumericKind {
    Int(i64),
    Float(f32),
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A function application in the IR.
///
/// This includes qualifications that were resolved from Qualif and PreQualified
/// expressions in the AST.
pub struct Apply<'cx> {
    /// The function being called.
    pub function: Expr<'cx>,
    /// The function arguments.
    pub args: &'cx [Expr<'cx>],
    /// Qualifications applied to this call (resolved from AST Qualif/PreQualified).
    pub qualifications: &'cx [syn::Qualif<'cx, IrTypeFamily>],
    /// The resolved function definition.
    pub resolved_function: Option<ResolvedPath<'cx>>,
}
