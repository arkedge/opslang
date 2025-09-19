/*!
AST for opslang v1.

This module defines the AST structures for opslang v1 syntax. The AST is designed to be
generic over a type family to support different contexts (parsing vs analysis).

**IMPORTANT**: When modifying type structures in this module, update visitor type
registries to ensure proper visitor macro generation:
- `opslang-ast-macro/src/visitor_type_registry.rs` for AST visitors
- `opslang-ir-macro/src/visitor_type_registry.rs` for IR visitors

Choose whether to make types hookable (add to node types) or not hookable (add to inter types).
*/

use std::fmt::Debug;

pub use family::TypeFamily;
use opslang_ast_macro::OrderSpan;
use opslang_visitor_macro::Visit;

pub mod ast_consistency_check;
pub mod context;
pub mod family;
pub mod loc;
pub mod token;
pub mod visit;

pub mod constructors;
pub mod impls;

impl Versioned for Program<'_, DefaultTypeFamily> {
    type Version = V1;
}

#[derive(Debug, PartialEq, Clone, Copy, Default, Visit)]
#[skip_all_visit]
/// Default value for each types in this crate, to allow this crate define an AST.
///
/// This type does not take any lifetime parameters because [`TypeFamily`] trait has them.
pub struct DefaultTypeFamily;

impl<'cx> TypeFamily<'cx> for DefaultTypeFamily {
    opslang_ast_macro::v1_default_type_subst_internal! {
        ..
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
#[skip_all_visit]
pub struct BytePos(pub u32);
pub type Position = BytePos;

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
#[skip_all_visit]
/// A location in the code.
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// An overall program. A program is a sequence of function definitions and constant definitions.
pub struct Program<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub toplevel_items: &'cx [F::ToplevelItem],
}

/// A top-level item in a program.
///
/// This represents a single item at the top level of a program, which can be either
/// a definition (function or constant) or just a comment. Items can be empty when
/// they contain only whitespace or empty lines, which is valid in the AST representation.
#[derive(Debug, PartialEq, Visit)]
pub struct ToplevelItem<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub kind: Option<DefinitionKind<'cx, F>>,
    pub comment: Option<F::Comment>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A top-level definition in a program.
pub enum DefinitionKind<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    Function(F::FunctionDef),
    Constant(F::ConstantDef),
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A function definition with `prc` keyword.
///
/// # Examples
///
/// ```ops
/// prc main() {
///     NOP;
/// }
/// prc add(x: i32, y: i32) {
///     return x + y;
/// }
/// ```
pub struct FunctionDef<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub prc_token: token::Prc<'cx, F>,
    pub name: F::Ident,
    pub left_paren: token::OpenParen<'cx, F>,
    pub parameters: &'cx [Parameter<'cx, F>],
    pub right_paren: token::CloseParen<'cx, F>,
    pub return_type: F::FnReturnTy,
    pub body: F::Block,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A function parameter.
///
/// # Examples
///
/// ```ops
/// x: i32
/// ```
pub struct Parameter<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub name: F::Ident,
    pub colon: token::Colon<'cx, F>,
    pub ty: F::Ty,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct FnReturnTy<'cx, F: TypeFamily<'cx> = DefaultTypeFamily>(
    pub Option<(token::Arrow<'cx, F>, F::Path)>,
);

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A constant definition.
///
/// # Examples
///
/// ```ops
/// const CONSTANT: i32 = 0;
/// ```
pub struct ConstantDef<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub const_token: token::Const<'cx, F>,
    pub name: F::Ident,
    pub colon: token::Colon<'cx, F>,
    pub ty: F::Path,
    pub eq: token::Eq<'cx, F>,
    pub value: F::Expr,
    pub semi: token::Semi<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// Sequence of statements.
pub struct Scope<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub items: &'cx [ScopeItem<'cx, F>],
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A scope item can be a single statement or a block of statements, or a comment.
pub enum ScopeItem<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    Row(F::Row),
    Block(F::Block),
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A single row of program with optional comments and breaks.
pub struct Row<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub breaks: Option<token::Break<'cx, F>>,
    pub statement: Option<F::Statement>,
    pub comment: Option<F::Comment>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A comment in a program.
pub struct Comment<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    #[skip_visit]
    pub content: &'cx str,
    pub span: F::Span,
}

#[derive(Debug, PartialEq, Visit)]
/// A block of statements with optional comments and a default receiver component. A block can also have a delay.
///
/// # Examples
///
/// ```ops
/// {
///     NOP
///     NOP
/// }
/// ```
pub struct Block<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub left_brace: token::OpenBrace<'cx, F>,
    pub scope: F::Scope,
    pub right_brace: token::CloseBrace<'cx, F>,
}

#[derive(Debug, PartialEq, Visit)]
/// A statement kind.
pub enum Statement<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    Let(Let<'cx, F>),
    Expr(ExprStatement<'cx, F>),
    Return(F::ReturnStmt),
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A let statement.
///
/// # Examples
///
/// ```ops
/// let d = 1s
/// ```
pub struct Let<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub let_token: token::Let<'cx, F>,
    pub variable: F::Ident,
    pub eq: token::Eq<'cx, F>,
    pub rhs: F::Expr,
    pub semi: token::Semi<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A statement kind.
pub struct ExprStatement<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub expr: F::Expr,
    pub semi: token::Semi<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, opslang_ast_macro::MapIntoToken)]
/// A `return` statement.
///
/// # Examples
///
/// ```ops
/// return;
/// ```
pub struct ReturnStmt<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub return_token: token::Return<'cx, F>,
    pub semi: token::Semi<'cx, F>,
}

pub type OwnedExpr<'cx, F = DefaultTypeFamily> = ExprKind<'cx, F>;

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// An expression.
pub enum ExprKind<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    Variable(F::Path),
    Literal(F::Literal),
    Parened(F::Parened),
    Qualif(F::Qualif),
    PreQualified(F::PreQualified),
    Unary(F::Unary),
    Compare(F::Compare),
    Binary(F::Binary),
    Apply(F::Apply),
    Set(F::Set),
    Cast(F::Cast),
    InfixImport(F::InfixImport),
    If(F::If),
    Wait(F::Wait),
    Select(F::Select),
}

mod sealed {
    #[derive(Clone, Copy)]
    pub struct Sealed;
}

/// A newtyped [`ExprKind`]. Use this type instead of [`ExprKind`] whenever possible.
pub struct Expr<'cx, F: TypeFamily<'cx> = DefaultTypeFamily>(
    pub &'cx ExprKind<'cx, F>,
    sealed::Sealed,
);

/// Unused type in Ast, but used in Ir crate.
pub struct ExprMut<'cx, F: TypeFamily<'cx>>(pub &'cx mut ExprKind<'cx, F>, sealed::Sealed);

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct Path<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub segments: &'cx [F::Ident],
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct Ident<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    #[skip_visit]
    pub raw: &'cx str,
    pub span: F::Span,
}

#[derive(Debug, PartialEq, Visit)]
/// A qualification for a function application.
///
/// The qualification system enables flexible function argument modification and assignment.
/// This system abstracts argument handling at a higher level than traditional named parameter
/// systems found in languages like OCaml.
///
/// ## Core Concepts
///
/// **Modifiers**: Argument specification constructs that allow functions to accept parameters of
/// specific types (numeric or DateTime). Modifiers are provided by modules and enable global
/// abstraction of same-named arguments across different functions.
/// - Syntax: `@TL:20` where `TL` is the modifier name and `:20` is the parameter
///
/// **Default Modifiers**: Provide unnamed optional arguments that can be applied without explicit
/// parameters, offering a simplified qualification syntax.
/// - Syntax: `~MOBC` where `MOBC` is the default modifier value
///
/// ## Key Features
///
/// 1. **Global Argument Abstraction**: Unlike function-specific named parameters, the qualification
///    system provides module-level argument abstraction, allowing the same argument names to be
///    reused across multiple functions.
///
/// 2. **Flexible Parameter Assignment**: The system facilitates easy argument substitution and
///    modification, making function application more intuitive.
///
/// 3. **Module-Based Provision**: Modifiers are provided by modules, creating a systematic approach
///    to argument specification across the language ecosystem.
///
/// ## Example Usage
///
/// ```ops
/// AOBC.NOP @TL:20 ~MOBC
/// ```
///
/// In this example:
/// - `@TL:20` is a `Modifier` with parameter `20`
/// - `~MOBC` is a `DefaultModifier`
/// - Both qualify the function call `AOBC.NOP`
pub enum Qualif<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    Modifier(Modifier<'cx, F>),
    DefaultModifier(DefaultModifier<'cx, F>),
}

#[derive(Debug, PartialEq, Visit)]
/// A modifier for command argument specification.
///
/// See [`Qualif`] for more information.
///
/// # Examples
///
/// - `@TL:20` in `AOBC.NOP @TL:20 ~MOBC`.
pub struct Modifier<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub at_token: token::Atmark<'cx, F>,
    pub id: F::Path,
    pub arg: Option<ModifierParam<'cx, F>>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A parameter for a command modifier.
///
/// See [`Qualif`] for more information.
///
/// # Examples
///
/// - `:20` in `@TL:20`.
pub struct ModifierParam<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub colon_token: token::Colon<'cx, F>,
    pub value: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
/// A default modifier for command without explicit parameters.
///
/// See [`Qualif`] for more information.
///
/// # Examples
///
/// - `~MOBC` in `AOBC.NOP @TL:20 ~MOBC`.
pub struct DefaultModifier<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub tilde_token: token::Tilde<'cx, F>,
    pub value: F::Path,
}

pub use literal::*;

use crate::{V1, version::Versioned};

pub mod literal {
    use super::*;

    #[derive(Debug, PartialEq, Clone, Copy, Visit)]
    pub enum Literal<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        Array(F::Array),
        String(F::String),
        Bytes(F::Bytes),
        HexBytes(F::HexBytes),
        Numeric(F::Numeric),
        DateTime(F::DateTime),
    }

    #[derive(Debug, PartialEq, Clone, Copy, Visit)]
    pub struct Array<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        pub left_bracket: token::OpenSquare<'cx, F>,
        pub exprs: F::Exprs,
        pub right_bracket: token::CloseSquare<'cx, F>,
    }

    #[derive(Debug, PartialEq, Clone, Copy, Visit)]
    pub struct String<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        #[skip_visit]
        pub raw: &'cx str,
        pub span: F::Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy, Visit)]
    pub struct Bytes<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        #[skip_visit]
        pub raw: &'cx str,
        pub span: F::Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy, Visit)]
    pub struct HexBytes<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        #[skip_visit]
        pub raw: &'cx str,
        pub span: F::Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy, Visit)]
    pub struct Numeric<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        #[skip_visit]
        /// The raw string representation of the numeric value, without any prefix or suffix.
        pub raw: &'cx str,

        pub kind: NumericKind,
        pub suffix: Option<NumericSuffix<'cx, F>>,
    }

    #[derive(Debug, PartialEq, Clone, Copy, Visit)]
    #[skip_all_visit]
    pub enum NumericKind {
        Integer(IntegerPrefix),
        Float,
    }

    #[derive(Debug, PartialEq, Clone, Copy, Visit)]
    /// Suffix of numeral value. Allows any ident at this point.
    pub struct NumericSuffix<'cx, F: TypeFamily<'cx> = DefaultTypeFamily>(pub F::Ident);

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum IntegerPrefix {
        /// `0x`
        Hexadecimal,

        /// `0o`
        Octal,

        /// `0b`
        Binary,

        None,
    }

    #[derive(Debug, PartialEq, Clone, Copy, Visit)]
    /// A date-time value.
    pub struct DateTime<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        #[skip_visit]
        pub raw: &'cx str,
        pub span: F::Span,
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct Parened<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub left_paren: token::OpenParen<'cx, F>,
    pub expr: F::Expr,
    pub right_paren: token::CloseParen<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct PreQualified<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub qualifs: &'cx [F::Qualif],
    pub expr: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct Unary<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub op: UnOp<'cx, F>,
    pub expr: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, opslang_ast_macro::MapIntoToken)]
pub enum UnOp<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    /// Negates an expression.
    Neg(token::Hyphen<'cx, F>),

    /// Create a reference of an expression.
    ///
    /// This is a temporal solution for accepting the old `tlmid!` functionality.
    IdRef(token::Ampersand<'cx, F>),

    Deref(token::Dollar<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct Compare<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub head: F::Expr,
    pub tail_with_op: &'cx [CompareOpExpr<'cx, F>],
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct CompareOpExpr<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub op: CompareOp<'cx, F>,
    pub val: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, opslang_ast_macro::MapIntoToken)]
pub enum CompareOp<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    GreaterEq(token::RightAngleEq<'cx, F>),
    LessEq(token::AngleEq<'cx, F>),
    Greater(token::RightAngle<'cx, F>),
    Less(token::Angle<'cx, F>),
    NotEqual(NotEqualToken<'cx, F>),
    Equal(token::EqualEqual<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, opslang_ast_macro::MapIntoToken)]
pub enum NotEqualToken<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    /// `!=`
    BangEqual(token::BangEqual<'cx, F>),
    /// `/=`
    SlashEqual(token::SlashEqual<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct Binary<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub lhs: F::Expr,
    pub op: F::BinOp,
    pub rhs: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, opslang_ast_macro::MapIntoToken)]
pub enum BinOp<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    /// `&&`
    And(token::AndAnd<'cx, F>),
    /// `||`
    Or(token::OrOr<'cx, F>),
    /// `*`
    Mul(token::Star<'cx, F>),
    /// `/`
    Div(token::Slash<'cx, F>),
    /// `%`
    Mod(token::Percent<'cx, F>),
    /// `+`
    Add(token::Plus<'cx, F>),
    /// `-`
    Sub(token::Hyphen<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy, Visit)]
pub struct Apply<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub function: F::Expr,
    pub args: &'cx [F::Expr],
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, OrderSpan)]
pub struct Set<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub lhs: F::Expr,
    pub colon_eq: token::ColonEq<'cx, F>,
    pub rhs: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, OrderSpan)]
pub struct Cast<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub expr: F::Expr,
    pub as_kw: token::As<'cx, F>,
    pub ty: F::Ty,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, OrderSpan)]
pub struct InfixImport<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub file: F::Expr,
    pub question: token::Question<'cx, F>,
    pub path: F::Path,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, OrderSpan)]
pub struct If<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub if_kw: token::If<'cx, F>,
    pub cond: F::Expr,
    pub then_clause: F::Block,
    pub else_opt: Option<IfElse<'cx, F>>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, OrderSpan)]
pub struct IfElse<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub else_kw: token::Else<'cx, F>,
    pub else_clause: F::Block,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, OrderSpan)]
pub struct Wait<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub wait_kw: token::Wait<'cx, F>,
    pub expr: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, OrderSpan)]
pub struct Select<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub select_kw: token::Select<'cx, F>,
    pub left_brace: token::OpenBrace<'cx, F>,
    pub items: F::SelectItems,
    pub right_brace: token::CloseBrace<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy, Visit, OrderSpan)]
pub struct SelectItem<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub expr: F::Expr,
    pub arrow: token::DoubleArrow<'cx, F>,
    pub body: F::Block,
}
