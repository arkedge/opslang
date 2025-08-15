use std::fmt::Debug;

pub use family::TypeFamily;
use opslang_ast_macros::OrderSpan;

pub mod context;
pub mod family;
pub mod loc;
pub mod token;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BytePos(pub u32);
pub type Position = BytePos;

#[derive(Debug, PartialEq, Clone, Copy, Default)]
/// Default value for each types in this crate, to allow this crate define an AST.
///
/// This type does not take any lifetime parameters because [`TypeFamily`] trait has them.
pub struct DefaultTypeFamily;

#[macro_export]
/// Type substitution that makes [`DefaultTypeFamily`] default.
///
/// You can override some of the types by passing them as arguments.
/// The arguments must be provided in the order of the fields in this macro.
///
/// # Examples
///
/// ```rust,ignore
/// // Use all default types.
/// v1_default_type_subst! {}
///
/// // Override `Expr` type.
/// v1_default_type_subst! {
///     Expr = MyExpr,
/// }
/// ```
macro_rules! v1_default_type_subst {
    // Note: The order of overrides must match the order below.
    (
        $(Span = $span:ty,)?
        $(Position = $position:ty,)?
        $(Comment = $comment:ty,)?
        $(Row = $row:ty,)?
        $(Statement = $statement:ty,)?
        $(Block = $block:ty,)?
        $(ScopeItem = $scope_item:ty,)?
        $(ReturnStmt = $return_stmt:ty,)?
        $(Ident = $ident:ty,)?
        $(Path = $path:ty,)?
        $(Expr = $expr:ty,)?
        $(Qualif = $qualif:ty,)?
        $(PreQualified = $pre_qualified:ty,)?
        $(Parened = $parened:ty,)?
        $(Literal = $literal:ty,)?
        $(Array = $array:ty,)?
        $(String = $string:ty,)?
        $(Bytes = $bytes:ty,)?
        $(HexBytes = $hex_bytes:ty,)?
        $(DateTime = $date_time:ty,)?
        $(Numeric = $numeric:ty,)?
        $(Apply = $apply:ty,)?
        $(Unary = $unary:ty,)?
        $(Binary = $binary:ty,)?
        $(Compare = $compare:ty,)?
        $(Set = $set:ty,)?
        $(InfixImport = $infix_import:ty,)?
        $(If = $if:ty,)?
        $(FunctionDef = $function_def:ty,)?
        $(ConstantDef = $constant_def:ty,)?
        ..
    ) => {
        type Span = v1_default_type_subst!{
            $crate::syntax::v1::Span;
            [$($span)?]
        };
        type Position = v1_default_type_subst!{
            $crate::syntax::v1::Position;
            [$($position)?]
        };
        type Comment = v1_default_type_subst!{
            &'cx $crate::syntax::v1::Comment<'cx, Self>;
            [$($comment)?]
        };
        type Row = v1_default_type_subst!{
            &'cx $crate::syntax::v1::Row<'cx, Self>;
            [$($row)?]
        };
        type Statement = v1_default_type_subst!{
            $crate::syntax::v1::Statement<'cx, Self>;
            [$($statement)?]
        };
        type Block = v1_default_type_subst!{
            &'cx $crate::syntax::v1::Block<'cx, Self>;
            [$($block)?]
        };
        type ScopeItem = v1_default_type_subst!{
            $crate::syntax::v1::ScopeItem<'cx, Self>;
            [$($scope_item)?]
        };
        type ReturnStmt = v1_default_type_subst!{
            $crate::syntax::v1::ReturnStmt<'cx, Self>;
            [$($return_stmt)?]
        };
        type Ident = v1_default_type_subst!{
            $crate::syntax::v1::Ident<'cx, Self>;
            [$($ident)?]
        };
        type Path = v1_default_type_subst!{
            $crate::syntax::v1::Path<'cx, Self>;
            [$($path)?]
        };
        type Expr = v1_default_type_subst!{
            $crate::syntax::v1::Expr<'cx, Self>;
            [$($expr)?]
        };
        type Qualif = v1_default_type_subst!{
            $crate::syntax::v1::Qualif<'cx, Self>;
            [$($qualif)?]
        };
        type PreQualified = v1_default_type_subst!{
            $crate::syntax::v1::PreQualified<'cx, Self>;
            [$($pre_qualified)?]
        };
        type Parened = v1_default_type_subst!{
            $crate::syntax::v1::Parened<'cx, Self>;
            [$($parened)?]
        };
        type Literal = v1_default_type_subst!{
            $crate::syntax::v1::Literal<'cx, Self>;
            [$($literal)?]
        };
        type Array = v1_default_type_subst!{
            $crate::syntax::v1::literal::Array<'cx, Self>;
            [$($array)?]
        };
        type String = v1_default_type_subst!{
            $crate::syntax::v1::literal::String<'cx, Self>;
            [$($string)?]
        };
        type Bytes = v1_default_type_subst!{
            $crate::syntax::v1::literal::Bytes<'cx, Self>;
            [$($bytes)?]
        };
        type HexBytes = v1_default_type_subst!{
            $crate::syntax::v1::literal::HexBytes<'cx, Self>;
            [$($hex_bytes)?]
        };
        type DateTime = v1_default_type_subst!{
            $crate::syntax::v1::literal::DateTime<'cx, Self>;
            [$($date_time)?]
        };
        type Numeric = v1_default_type_subst!{
            $crate::syntax::v1::literal::Numeric<'cx, Self>;
            [$($numeric)?]
        };
        type Apply = v1_default_type_subst!{
            $crate::syntax::v1::Apply<'cx, Self>;
            [$($apply)?]
        };
        type Unary = v1_default_type_subst!{
            $crate::syntax::v1::Unary<'cx, Self>;
            [$($unary)?]
        };
        type Binary = v1_default_type_subst!{
            $crate::syntax::v1::Binary<'cx, Self>;
            [$($binary)?]
        };
        type Compare = v1_default_type_subst!{
            $crate::syntax::v1::Compare<'cx, Self>;
            [$($compare)?]
        };
        type Set = v1_default_type_subst!{
            $crate::syntax::v1::Set<'cx, Self>;
            [$($set)?]
        };
        type InfixImport = v1_default_type_subst!{
            $crate::syntax::v1::InfixImport<'cx, Self>;
            [$($infix_import)?]
        };
        type If = v1_default_type_subst!{
            $crate::syntax::v1::If<'cx, Self>;
            [$($if)?]
        };
        type FunctionDef = v1_default_type_subst!{
            $crate::syntax::v1::FunctionDef<'cx, Self>;
            [$($function_def)?]
        };
        type ConstantDef = v1_default_type_subst!{
            $crate::syntax::v1::ConstantDef<'cx, Self>;
            [$($constant_def)?]
        };
    };
    // Internal
    // If the second argument (the user override) is present, use it.
    ($default:ty; [$user:ty]) => {
        $user
    };
    // If the second argument is empty, use the default.
    ($default:ty; []) => {
        $default
    };
}

impl<'cx> TypeFamily<'cx> for DefaultTypeFamily {
    v1_default_type_subst! {
        ..
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A location in the code.
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// An overall program. A program is a sequence of function definitions and constant definitions.
pub struct Program<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub definitions: &'cx [Definition<'cx, F>],
}

#[derive(Debug, PartialEq)]
pub struct Definition<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub kind: Option<DefinitionKind<'cx, F>>,
    pub comment: Option<F::Comment>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A top-level definition in a program.
pub enum DefinitionKind<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    Function(F::FunctionDef),
    Constant(F::ConstantDef),
}

impl<'cx, F: TypeFamily<'cx>> Definition<'cx, F> {
    pub fn is_empty(&self) -> bool {
        let Self { kind, comment } = self;
        kind.is_none() && comment.is_none()
    }
}

impl<'cx, F: TypeFamily<'cx>> Default for Definition<'cx, F> {
    fn default() -> Self {
        Self {
            kind: Default::default(),
            comment: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A function definition with `prec` keyword.
///
/// # Examples
///
/// ```ops
/// prec main() {
///     NOP;
/// }
/// prec add(x: i32, y: i32) {
///     return x + y;
/// }
/// ```
pub struct FunctionDef<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub proc_token: token::Prc<'cx, F>,
    pub name: F::Ident,
    pub left_paren: token::OpenParen<'cx, F>,
    pub parameters: &'cx [Parameter<'cx, F>],
    pub right_paren: token::CloseParen<'cx, F>,
    pub body: F::Block,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    pub ty: F::Path,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
}

impl Versioned for Program<'_, DefaultTypeFamily> {
    type Version = V1;
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// Sequence of statements.
pub struct Scope<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub items: &'cx [F::ScopeItem],
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A scope item can be a single statement or a block of statements, or a comment.
pub enum ScopeItem<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    Row(F::Row),
    Block(F::Block),
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A single row of program with optional comments and breaks.
pub struct Row<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub breaks: Option<token::Break<'cx, F>>,
    pub statement: Option<F::Statement>,
    pub comment: Option<F::Comment>,
}

impl<'cx, F: TypeFamily<'cx>> Row<'cx, F> {
    pub fn is_empty(&self) -> bool {
        let Self {
            breaks,
            statement,
            comment,
        } = self;
        breaks.is_none() && statement.is_none() && comment.is_none()
    }
}

impl<'cx, F: TypeFamily<'cx>> Default for Row<'cx, F> {
    fn default() -> Self {
        Self {
            breaks: Default::default(),
            statement: Default::default(),
            comment: Default::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A comment in a program.
pub struct Comment<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub content: &'cx str,
    pub span: F::Span,
}

impl<'cx, F: TypeFamily<'cx>> Comment<'cx, F> {
    #[inline]
    pub fn is_meta(&self) -> bool {
        self.content.starts_with('!')
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    pub scope: Scope<'cx, F>,
    pub right_brace: token::CloseBrace<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A statement kind.
pub enum Statement<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    Let(Let<'cx, F>),
    Expr(ExprStatement<'cx, F>),
    Return(F::ReturnStmt),
}

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
/// A statement kind.
pub struct ExprStatement<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub expr: F::Expr,
    pub semi: token::Semi<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy, opslang_ast_macros::MapIntoToken)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
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
    InfixImport(F::InfixImport),
    If(F::If),
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

impl<'cx, F: TypeFamily<'cx>> std::ops::Deref for Expr<'cx, F> {
    type Target = &'cx ExprKind<'cx, F>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cx, F: TypeFamily<'cx>> Debug for Expr<'cx, F> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl<'cx, F: TypeFamily<'cx>> PartialEq for Expr<'cx, F> {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq(other.0)
    }
}

impl<'cx, F: TypeFamily<'cx>> Clone for Expr<'cx, F> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'cx, F: TypeFamily<'cx>> Copy for Expr<'cx, F> {}

impl<'cx, F: TypeFamily<'cx>> Expr<'cx, F> {
    #[inline]
    pub fn from_kind(ctx: &'cx context::Context<'cx, F>, kind: ExprKind<'cx, F>) -> Self {
        ctx.alloc_expr(kind)
    }

    #[inline]
    pub fn ident(ctx: &'cx context::Context<'cx, F>, name: &str, span: F::Span) -> Self
    where
        F: TypeFamily<'cx, Path = Path<'cx, F>>,
    {
        Expr::variable(ctx, Path::single(ctx, name, span))
    }

    #[inline]
    pub fn variable(ctx: &'cx context::Context<'cx, F>, path: F::Path) -> Self {
        ctx.alloc_expr(ExprKind::Variable(path))
    }

    #[inline]
    pub fn literal(ctx: &'cx context::Context<'cx, F>, literal: F::Literal) -> Self {
        ctx.alloc_expr(ExprKind::Literal(literal))
    }

    #[inline]
    pub fn parened(
        ctx: &'cx context::Context<'cx, F>,
        left_paren: token::OpenParen<'cx, F>,
        expr: F::Expr,
        right_paren: token::CloseParen<'cx, F>,
    ) -> Self
    where
        F: TypeFamily<'cx, Parened = Parened<'cx, F>>,
    {
        let parened = Parened {
            left_paren,
            expr,
            right_paren,
        };
        ctx.alloc_expr(ExprKind::Parened(parened))
    }

    #[inline]
    pub fn apply(ctx: &'cx context::Context<'cx, F>, function: F::Expr, args: Vec<F::Expr>) -> Self
    where
        F: TypeFamily<'cx, Apply = Apply<'cx, F>>,
    {
        let apply = Apply {
            function,
            args: ctx.alloc_expr_slice(args),
        };
        ctx.alloc_expr(ExprKind::Apply(apply))
    }

    #[inline]
    pub fn binary(
        ctx: &'cx context::Context<'cx, F>,
        lhs: F::Expr,
        op: BinOp<'cx, F>,
        rhs: F::Expr,
    ) -> Self
    where
        F: TypeFamily<'cx, Binary = Binary<'cx, F>>,
    {
        let binary = Binary { lhs, op, rhs };
        ctx.alloc_expr(ExprKind::Binary(binary))
    }

    #[inline]
    pub fn unary(ctx: &'cx context::Context<'cx, F>, op: UnOp<'cx, F>, expr: F::Expr) -> Self
    where
        F: TypeFamily<'cx, Unary = Unary<'cx, F>>,
    {
        let unary = Unary { op, expr };
        ctx.alloc_expr(ExprKind::Unary(unary))
    }

    #[inline]
    pub fn compare(
        ctx: &'cx context::Context<'cx, F>,
        head: F::Expr,
        tail_with_op: Vec<(CompareOp<'cx, F>, F::Expr)>,
    ) -> Self
    where
        F: TypeFamily<'cx, Compare = Compare<'cx, F>>,
    {
        let compare = Compare {
            head,
            tail_with_op: ctx.alloc_compare_op_expr_tuple_slice(tail_with_op),
        };
        ctx.alloc_expr(ExprKind::Compare(compare))
    }

    #[inline]
    pub fn compare_single(
        ctx: &'cx context::Context<'cx, F>,
        lhs: F::Expr,
        op: CompareOp<'cx, F>,
        rhs: F::Expr,
    ) -> Self
    where
        F: TypeFamily<'cx, Compare = Compare<'cx, F>>,
    {
        Self::compare(ctx, lhs, vec![(op, rhs)])
    }

    #[inline]
    pub fn set(
        ctx: &'cx context::Context<'cx, F>,
        lhs: F::Expr,
        colon_eq: token::ColonEq<'cx, F>,
        rhs: F::Expr,
    ) -> Self
    where
        F: TypeFamily<'cx, Set = Set<'cx, F>>,
    {
        let set = Set { lhs, colon_eq, rhs };
        ctx.alloc_expr(ExprKind::Set(set))
    }

    #[inline]
    pub fn import(
        ctx: &'cx context::Context<'cx, F>,
        file: F::Expr,
        question: token::Question<'cx, F>,
        path: F::Path,
    ) -> Self
    where
        F: TypeFamily<'cx, InfixImport = InfixImport<'cx, F>>,
    {
        let import = InfixImport {
            file,
            question,
            path,
        };
        ctx.alloc_expr(ExprKind::InfixImport(import))
    }

    #[inline]
    pub fn if_then_else(
        ctx: &'cx context::Context<'cx, F>,
        if_kw: token::If<'cx, F>,
        cond: F::Expr,
        then_clause: F::Block,
        else_kw: token::Else<'cx, F>,
        else_clause: F::Block,
    ) -> Self
    where
        F: TypeFamily<'cx, If = If<'cx, F>>,
    {
        let if_expr = If {
            if_kw,
            cond,
            then_clause,
            else_opt: Some(IfElse {
                else_kw,
                else_clause,
            }),
        };
        ctx.alloc_expr(ExprKind::If(if_expr))
    }
    #[inline]
    pub fn if_then(
        ctx: &'cx context::Context<'cx, F>,
        if_kw: token::If<'cx, F>,
        cond: F::Expr,
        then_clause: F::Block,
    ) -> Self
    where
        F: TypeFamily<'cx, If = If<'cx, F>>,
    {
        let if_expr = If {
            if_kw,
            cond,
            then_clause,
            else_opt: None,
        };
        ctx.alloc_expr(ExprKind::If(if_expr))
    }
    #[inline]
    pub fn if_expr(
        ctx: &'cx context::Context<'cx, F>,
        if_kw: token::If<'cx, F>,
        cond: F::Expr,
        then_clause: F::Block,
        else_opt: Option<IfElse<'cx, F>>,
    ) -> Self
    where
        F: TypeFamily<'cx, If = If<'cx, F>>,
    {
        let if_expr = If {
            if_kw,
            cond,
            then_clause,
            else_opt,
        };
        ctx.alloc_expr(ExprKind::If(if_expr))
    }

    #[inline]
    pub fn qualif(ctx: &'cx context::Context<'cx, F>, qualif: F::Qualif) -> Self {
        ctx.alloc_expr(ExprKind::Qualif(qualif))
    }

    #[inline]
    pub fn pre_qualified(
        ctx: &'cx context::Context<'cx, F>,
        qualifs: Vec<F::Qualif>,
        expr: F::Expr,
    ) -> Self
    where
        F: TypeFamily<'cx, PreQualified = PreQualified<'cx, F>>,
    {
        let pre_qualified = PreQualified {
            qualifs: ctx.alloc_qualif_slice(qualifs),
            expr,
        };
        ctx.alloc_expr(ExprKind::PreQualified(pre_qualified))
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Path<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub raw: &'cx str,
    pub segments: &'cx [Ident<'cx, F>],
}

impl<'cx, F: TypeFamily<'cx>> Path<'cx, F> {
    pub fn new_unchecked(
        ctx: &'cx context::Context<'cx, F>,
        raw: &str,
        segments: &'cx [Ident<'cx, F>],
    ) -> Self {
        let raw_str = ctx.alloc_str(raw);
        Path {
            raw: raw_str,
            segments,
        }
    }

    pub fn single(ctx: &'cx context::Context<'cx, F>, name: &str, span: F::Span) -> Self {
        assert!(!name.contains('.'));
        let ident = Ident::new(ctx, name, span);
        let segments = ctx.alloc_ident_slice(vec![ident]);
        Path::new_unchecked(ctx, name, segments)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Ident<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub raw: &'cx str,
    pub span: F::Span,
}

impl<'cx, F: TypeFamily<'cx>> Ident<'cx, F> {
    pub fn new(
        ctx: &'cx context::Context<'cx, impl TypeFamily<'cx>>,
        name: &str,
        span: F::Span,
    ) -> Self {
        let name_str = ctx.alloc_str(name);
        Ident {
            raw: name_str,
            span,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A qualification for a function application.
///
/// The OpLang qualification system enables flexible function argument modification and assignment.
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
/// - Syntax: `~MOBC` where `MOBC` is the default modifier name
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

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
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

#[derive(Debug, PartialEq, Clone, Copy)]
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

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Literal<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        Array(F::Array),
        String(F::String),
        Bytes(F::Bytes),
        HexBytes(F::HexBytes),
        Numeric(F::Numeric),
        DateTime(F::DateTime),
    }

    impl<'cx, F: TypeFamily<'cx>> Literal<'cx, F> {
        pub fn string(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
            content: &str,
            span: F::Span,
        ) -> Self
        where
            F: TypeFamily<'cx, String = String<'cx, F>>,
        {
            let string_str = ctx.alloc_str(content);
            Literal::String(String {
                raw: string_str,
                span,
            })
        }

        pub fn bytes(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
            content: &str,
            span: F::Span,
        ) -> Self
        where
            F: TypeFamily<'cx, Bytes = Bytes<'cx, F>>,
        {
            let bytes_str = ctx.alloc_str(content);
            Literal::Bytes(Bytes {
                raw: bytes_str,
                span,
            })
        }

        pub fn hex_bytes(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
            content: &str,
            span: F::Span,
        ) -> Self
        where
            F: TypeFamily<'cx, HexBytes = HexBytes<'cx, F>>,
        {
            let hex_str = ctx.alloc_str(content);
            Literal::HexBytes(HexBytes { raw: hex_str, span })
        }

        pub fn numeric(numeric: F::Numeric) -> Self
        where
            F: TypeFamily<'cx, Numeric = Numeric<'cx>>,
        {
            Literal::Numeric(numeric)
        }

        pub fn date_time(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
            content: &str,
            span: F::Span,
        ) -> Self
        where
            F: TypeFamily<'cx, DateTime = DateTime<'cx, F>>,
        {
            let date_str = ctx.alloc_str(content);
            Literal::DateTime(DateTime {
                raw: date_str,
                span,
            })
        }

        pub fn array(
            left_bracket: token::OpenSquare<'cx, F>,
            exprs: &'cx [F::Expr],
            right_bracket: token::CloseSquare<'cx, F>,
        ) -> Self
        where
            F: TypeFamily<'cx, Array = Array<'cx, F>>,
        {
            Literal::Array(Array {
                left_bracket,
                exprs,
                right_bracket,
            })
        }
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct Array<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        pub left_bracket: token::OpenSquare<'cx, F>,
        pub exprs: &'cx [F::Expr],
        pub right_bracket: token::CloseSquare<'cx, F>,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct String<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        pub raw: &'cx str,
        pub span: F::Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct Bytes<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        pub raw: &'cx str,
        pub span: F::Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct HexBytes<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        pub raw: &'cx str,
        pub span: F::Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct Numeric<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        /// The raw string representation of the numeric value, without any prefix or suffix.
        pub raw: &'cx str,

        pub kind: NumericKind,
        pub suffix: Option<NumericSuffix<'cx, F>>,
    }

    impl<'cx, F: TypeFamily<'cx>> Numeric<'cx, F> {
        #[inline]
        pub fn integer(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, impl TypeFamily<'cx>>,
            raw: &str,
            prefix: IntegerPrefix,
            suffix: Option<NumericSuffix<'cx, F>>,
        ) -> Self {
            let raw_str = ctx.alloc_str(raw);
            Numeric {
                raw: raw_str,
                kind: NumericKind::Integer(prefix),
                suffix,
            }
        }

        #[inline]
        pub fn float(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
            raw: &str,
            suffix: Option<NumericSuffix<'cx, F>>,
        ) -> Self {
            let raw_str = ctx.alloc_str(raw);
            Numeric {
                raw: raw_str,
                kind: NumericKind::Float,
                suffix,
            }
        }

        #[inline]
        pub fn suffix(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
            name: &str,
            span: F::Span,
        ) -> NumericSuffix<'cx, F> {
            let name_str = ctx.alloc_str(name);
            NumericSuffix(Ident {
                raw: name_str,
                span,
            })
        }
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum NumericKind {
        Integer(IntegerPrefix),
        Float,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    /// Suffix of numeral value. Allows any ident at this point.
    pub struct NumericSuffix<'cx, F: TypeFamily<'cx>>(pub Ident<'cx, F>);

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

    #[derive(Debug, PartialEq, Clone, Copy)]
    /// A date-time value.
    pub struct DateTime<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        pub raw: &'cx str,
        pub span: F::Span,
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Parened<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub left_paren: token::OpenParen<'cx, F>,
    pub expr: F::Expr,
    pub right_paren: token::CloseParen<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct PreQualified<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub qualifs: &'cx [F::Qualif],
    pub expr: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Unary<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub op: UnOp<'cx, F>,
    pub expr: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, opslang_ast_macros::MapIntoToken)]
pub enum UnOp<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    /// Negates an expression.
    Neg(token::Hyphen<'cx, F>),

    /// Create a reference of an expression.
    ///
    /// This is a temporal solution for accepting the old `tlmid!` functionality.
    IdRef(token::Ampersand<'cx, F>),

    Deref(token::Dollar<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Compare<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub head: F::Expr,
    pub tail_with_op: &'cx [(CompareOp<'cx, F>, F::Expr)],
}

#[derive(Debug, PartialEq, Clone, Copy, opslang_ast_macros::MapIntoToken)]
pub enum CompareOp<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    GreaterEq(token::RightAngleEq<'cx, F>),
    LessEq(token::AngleEq<'cx, F>),
    Greater(token::RightAngle<'cx, F>),
    Less(token::Angle<'cx, F>),
    NotEqual(NotEqualToken<'cx, F>),
    Equal(token::EqualEqual<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy, opslang_ast_macros::MapIntoToken)]
pub enum NotEqualToken<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    /// `!=`
    BangEqual(token::BangEqual<'cx, F>),
    /// `/=`
    SlashEqual(token::SlashEqual<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Binary<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub lhs: F::Expr,
    pub op: BinOp<'cx, F>,
    pub rhs: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, opslang_ast_macros::MapIntoToken)]
pub enum BinOp<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    /// `&&`
    And(token::AndAnd<'cx, F>),
    /// `||`
    Or(token::OrOr<'cx, F>),
    /// `in`
    In(token::In<'cx, F>),
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Apply<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub function: F::Expr,
    pub args: &'cx [F::Expr],
}

#[derive(Debug, PartialEq, Clone, Copy, OrderSpan)]
pub struct Set<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub lhs: F::Expr,
    pub colon_eq: token::ColonEq<'cx, F>,
    pub rhs: F::Expr,
}

#[derive(Debug, PartialEq, Clone, Copy, OrderSpan)]
pub struct InfixImport<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub file: F::Expr,
    pub question: token::Question<'cx, F>,
    pub path: F::Path,
}

#[derive(Debug, PartialEq, Clone, Copy, OrderSpan)]
pub struct If<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub if_kw: token::If<'cx, F>,
    pub cond: F::Expr,
    pub then_clause: F::Block,
    pub else_opt: Option<IfElse<'cx, F>>,
}

#[derive(Debug, PartialEq, Clone, Copy, OrderSpan)]
pub struct IfElse<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub else_kw: token::Else<'cx, F>,
    pub else_clause: F::Block,
}
