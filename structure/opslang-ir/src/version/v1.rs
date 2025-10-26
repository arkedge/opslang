/*!
Intermediate Representation (IR) for opslang v1.

The IR serves as a bridge between the Abstract Syntax Tree (AST) and the type-checked representation.
This module defines the responsibility and structure of the IR layer.

**IMPORTANT**: When modifying type structures in this module, update the visitor type
registry in `opslang-ir-macro/src/visitor_type_registry.rs` to ensure proper visitor
macro generation. Choose whether to make types hookable (add to node types) or not
hookable (add to inter types).

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
    token::{IntoPosition, IntoSpan},
    v1::{self as ast, TypeFamily as AstTypeFamily, token},
};
use opslang_module::version::v1 as module;
use opslang_ty::version::v1::{self as ty, Ident, Ty, TypingContext};
use opslang_visitor_macro::Visit;
use std::convert::Infallible;

pub mod context;
pub use context::Context;
pub mod ir_consistency_check;

pub mod visit;
pub use visit::{IrMutVisitor, IrVisitor};

pub mod binop;
pub use binop::*;

#[derive(Debug, PartialEq, Clone, Copy, Default, Visit)]
#[skip_all_visit]
/// IR type family that includes type information and resolved names.
pub struct IrTypeFamily;

impl<'cx> AstTypeFamily<'cx> for IrTypeFamily {
    opslang_ast_macro::v1_default_type_subst! {
        Span = Option<ast::Span>,
        Position = Option<ast::Position>,

        // Core structural types
        Comment = Comment<'cx>,
        ToplevelItem = Definition<'cx>,
        Row = ast::Row<'cx, Self>,
        Block = ast::Block<'cx, Self>,
        Scope = Scope<'cx>,
        SelectItems = Vec<SelectItem<'cx>>,

        // Resolved name types
        Ident = Ident<'cx>,
        Path = ResolvedPath<'cx>,
        Ty = Ty<'cx>,
        FnReturnTy = Ty<'cx>,

        // Typed expression
        Expr = Expr<'cx>,
        Exprs = Vec<Expr<'cx>>,

        // Infallible qualification types (resolved into Apply)
        Qualif = Infallible,
        PreQualified = Infallible,

        // Expression components
        FilePath = module::ModulePath<'cx>,
        ImportedPath = Ident<'cx>,
        String = String<'cx>,
        Bytes = Bytes<'cx>,
        HexBytes = HexBytes<'cx>,
        DateTime = DateTime<'cx>,
        Numeric = Numeric<'cx>,
        Apply = Apply<'cx>,

        BinOp = BinOp<'cx>,
        Compare = Compare<'cx>,

        // Parentheses are removed
        Parened = Infallible,
        ..
    }
}

/// Re-export AST types with IrTypeFamily applied for convenience.
///
/// Use this types instead of `ast::{type}<'cx, IrTypeFamily>` for brevity and clarity.
macro_rules! re_export {
    ($(pub type $ty:ident<'cx>;)*) => {
        $(
            pub type $ty<'cx> = ast::$ty<'cx, IrTypeFamily>;
        )*
    };
}

pub type AstContext<'cx> = ast::context::Context<'cx, IrTypeFamily>;

re_export! {
    pub type Program<'cx>;

    pub type ToplevelItem<'cx>;
    pub type DefinitionKind<'cx>;

    pub type FunctionDef<'cx>;
    pub type Parameter<'cx>;
    pub type ConstantDef<'cx>;

    pub type ScopeItem<'cx>;

    pub type Row<'cx>;
    pub type Statement<'cx>;
    pub type Block<'cx>;
    pub type ReturnStmt<'cx>;

    pub type ExprKind<'cx>;
    pub type ExprMut<'cx>;
    pub type Qualif<'cx>;
    pub type ModifierParam<'cx>;
    pub type Modifier<'cx>;
    pub type DefaultModifier<'cx>;
    pub type Let<'cx>;
    pub type ExprStatement<'cx>;

    pub type Literal<'cx>;
    pub type Array<'cx>;

    pub type Unary<'cx>;
    pub type Binary<'cx>;
    pub type CompareOp<'cx>;
    pub type CompareOpExpr<'cx>;
    pub type Set<'cx>;
    pub type InfixImport<'cx>;
    pub type If<'cx>;
    pub type Select<'cx>;
    pub type SelectItem<'cx>;
}

impl<'cx> IntoSpan<'cx, IrTypeFamily> for ast::Span {
    fn into_span(self) -> <IrTypeFamily as AstTypeFamily<'cx>>::Span {
        Some(self)
    }
}

impl<'cx> IntoPosition<'cx, IrTypeFamily> for ast::Position {
    fn into_position(self) -> <IrTypeFamily as AstTypeFamily<'cx>>::Position {
        Some(self)
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A merged comment block in the IR.
///
/// Unlike AST comments which are stored line-by-line, IR comments represent
/// merged blocks of adjacent comments, which is more natural for multi-line
/// comment blocks in the source code.
pub struct Comment<'cx> {
    #[skip_visit]
    /// The combined content of all adjacent comments in the block.
    pub content: &'cx str,
    /// The span covering all merged comments.
    pub span: ast::Span,
    /// References to the original AST comments that were merged.
    pub source_comments: &'cx [&'cx ast::Comment<'cx>],
}

/// A top-level definition in the IR.
///
/// Unlike AST ToplevelItem, IR definitions cannot be empty and must contain
/// a concrete definition kind. Comments are split into leading and trailing
/// to provide better structure for code generation and analysis.
#[derive(Debug, PartialEq, Visit)]
pub struct Definition<'cx> {
    /// Comment appearing before this definition.
    pub comment_before: Option<Comment<'cx>>,
    /// Comment appearing after this definition on the same line.
    pub comment_trailing: Option<Comment<'cx>>,
    /// The actual definition content.
    pub kind: DefinitionKind<'cx>,
}

#[derive(Debug, PartialEq, Visit)]
/// A sequence of statements in the IR with mutable data storage.
pub struct Scope<'cx> {
    /// The statements in this scope, stored as a vector for mutability.
    pub items: Vec<ScopeItem<'cx>>,
}

#[derive(Debug, Clone, Copy, Visit, PartialEq)]
/// A resolved path reference to a definition or module item.
pub struct ResolvedPath<'cx> {
    /// The resolved item.
    pub item: ResolvedItem<'cx>,
    /// The original path that was resolved.
    pub original_path: &'cx ast::Path<'cx>,
}

#[derive(Debug, Clone, Copy, Visit, PartialEq)]
/// The resolved item - either a module item, local variable, or external reference.
pub enum ResolvedItem<'cx> {
    /// Reference to a module item (types, functions, constants from modules).
    ModuleItem(opslang_module::version::v1::ModuleItem<'cx>),
    /// Reference to a local variable (function parameters, local bindings).
    LocalVariable(opslang_ty::version::v1::Ident<'cx>),
    /// Reference to an external path resolved via an external resolver.
    External,
    /// Temporary placeholder for unresolved external functions during type checking.
    ///
    /// FIXME: This should be removed once proper module system are implemented.
    Main,
}

#[derive(Debug, PartialEq, Visit)]
/// A typed expression in the IR.
///
/// This is the core difference from AST expressions - IR expressions carry
/// type information from the type inference process.
pub struct Expr<'cx> {
    /// The expression kind/content.
    pub kind: ExprMut<'cx>,
    /// The inferred type of this expression.
    pub ty: Ty<'cx>,
}

impl<'cx> Expr<'cx> {
    pub fn new(kind: ExprMut<'cx>, ty: Ty<'cx>) -> Self {
        Self { kind, ty }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A parsed string literal in the IR.
pub struct String<'cx> {
    #[skip_visit]
    /// The parsed string value (escape sequences processed).
    pub value: &'cx str,
    /// Reference to the original AST string.
    pub ast: &'cx ast::literal::String<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A parsed bytes literal in the IR.
pub struct Bytes<'cx> {
    #[skip_visit]
    /// The parsed byte array.
    pub value: &'cx [u8],
    /// Reference to the original AST bytes literal.
    pub ast: &'cx ast::literal::Bytes<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A parsed hexadecimal bytes literal in the IR.
pub struct HexBytes<'cx> {
    #[skip_visit]
    /// The parsed byte array from hex representation.
    pub value: &'cx [u8],
    /// Reference to the original AST hex bytes literal.
    pub ast: &'cx ast::literal::HexBytes<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A parsed datetime literal in the IR.
pub struct DateTime<'cx> {
    #[skip_visit]
    /// The parsed datetime value.
    pub value: chrono::DateTime<Utc>,
    /// Reference to the original AST datetime literal.
    pub ast: &'cx ast::literal::DateTime<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A parsed numeric literal in the IR.
pub struct Numeric<'cx> {
    /// The value.
    pub kind: NumericKind<'cx>,
    /// Reference to the original AST numeric literal.
    pub ast: &'cx ast::literal::Numeric<'cx>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
#[skip_all_visit]
/// A parsed numeric literal in the IR.
pub enum NumericKind<'cx> {
    /// Signed integer with unified i64 representation.
    Int(i64),

    /// Unsigned integer with unified u64 representation.
    Uint(u64),

    /// Floating point with unified u64 bits representation.
    Float(u64),

    Duration(chrono::Duration),

    Repr(&'cx str),
}

#[derive(Debug, PartialEq, Visit)]
/// A function application in the IR.
///
/// This includes qualifications that were resolved from Qualif and PreQualified
/// expressions in the AST.
pub struct Apply<'cx> {
    /// The function being called.
    pub function: Expr<'cx>,
    /// The function arguments.
    pub args: Vec<Expr<'cx>>,
    /// Qualifications applied to this call (resolved from AST Qualif/PreQualified).
    pub qualifications: Vec<Qualif<'cx>>,
    /// The resolved function definition.
    pub resolved_function: Option<ResolvedPath<'cx>>,
}

#[derive(Debug, PartialEq, Visit)]
pub struct Compare<'cx> {
    pub head: Expr<'cx>,
    pub tail_with_op: Vec<CompareOpExpr<'cx>>,
}

/// Obtain the representative type of this node.
pub trait Typed<'cx> {
    /// The type of this node, can be optional if the node may not have a type.
    type Ty;

    /// Get the type of this node.
    fn ty(&self, cx: &'cx TypingContext<'cx>) -> Self::Ty;
}

impl<'cx, F: ast::TypeFamily<'cx>> Typed<'cx> for ast::Block<'cx, F> {
    type Ty = Option<Ty<'cx>>;
    fn ty(&self, _cx: &'cx TypingContext<'cx>) -> Self::Ty {
        None
    }
}

impl<'cx, F: ast::TypeFamily<'cx>> Typed<'cx> for ast::Scope<'cx, F> {
    type Ty = Option<Ty<'cx>>;
    fn ty(&self, _cx: &'cx TypingContext<'cx>) -> Self::Ty {
        // FIXME: the last item type
        None
    }
}

impl<'cx, F: ast::TypeFamily<'cx>> Typed<'cx> for ast::ScopeItem<'cx, F> {
    type Ty = Option<Ty<'cx>>;
    fn ty(&self, _cx: &'cx TypingContext<'cx>) -> Self::Ty {
        // FIXME: Block can have types
        None
    }
}

impl<'cx, F: ast::TypeFamily<'cx>> Typed<'cx> for ast::Statement<'cx, F> {
    type Ty = Ty<'cx>;
    fn ty(&self, cx: &'cx TypingContext<'cx>) -> Self::Ty {
        match self {
            ast::Statement::Let(let_stmt) => let_stmt.ty(cx),
            ast::Statement::Expr(expr_stmt) => expr_stmt.ty(cx),
            ast::Statement::Return(_) => Ty::mk_unit(cx),
        }
    }
}

impl<'cx, F: ast::TypeFamily<'cx>> Typed<'cx> for ast::Let<'cx, F> {
    type Ty = Ty<'cx>;
    fn ty(&self, cx: &'cx TypingContext<'cx>) -> Self::Ty {
        Ty::mk_unit(cx)
    }
}

impl<'cx, F: ast::TypeFamily<'cx>> Typed<'cx> for ast::ExprStatement<'cx, F> {
    type Ty = Ty<'cx>;
    fn ty(&self, cx: &'cx TypingContext<'cx>) -> Self::Ty {
        Ty::mk_unit(cx)
    }
}

impl<'cx> Typed<'cx> for Expr<'cx> {
    type Ty = Ty<'cx>;
    fn ty(&self, _cx: &'cx TypingContext<'cx>) -> Self::Ty {
        self.ty
    }
}

impl<'cx> Typed<'cx> for Scope<'cx> {
    type Ty = Option<Ty<'cx>>;
    fn ty(&self, _cx: &'cx TypingContext<'cx>) -> Self::Ty {
        // FIXME: the last item type
        None
    }
}
