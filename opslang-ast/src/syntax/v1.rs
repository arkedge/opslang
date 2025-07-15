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

impl<'cx> TypeFamily<'cx> for DefaultTypeFamily {
    type Span = Span;
    type Position = Position;

    type Comment = &'cx Comment<'cx>;
    type Row = &'cx Row<'cx>;
    type RowContent = StatementKind<'cx>;
    type Block = &'cx Block<'cx>;
    type ScopeItem = ScopeItem<'cx>;
    type ReturnStmt = ReturnStmt<'cx>;

    type Ident = Ident<'cx>;
    type Path = Path<'cx>;

    type Qualif = Qualif<'cx>;
    type PreQualified = PreQualified<'cx>;
    type Parened = Parened<'cx>;
    type Literal = Literal<'cx>;
    type Numeric = Numeric<'cx>;
    type Apply = Apply<'cx>;
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A location in the code.
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// An overall program. A program is a sequence of statements.
///
/// This version of program contains only a body of the main function.
pub struct Program<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub content: Scope<'cx, F>,
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
    pub content: Option<F::RowContent>,
    pub comment: Option<F::Comment>,
}

impl<'cx, F: TypeFamily<'cx>> Default for Row<'cx, F> {
    fn default() -> Self {
        Self {
            breaks: Default::default(),
            content: Default::default(),
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
pub enum StatementKind<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
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
    pub rhs: Expr<'cx, F>,
    pub semi: token::Semi<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A statement kind.
pub struct ExprStatement<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub expr: Expr<'cx, F>,
    pub semi: token::Semi<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    Unary(Unary<'cx, F>),
    Compare(Compare<'cx, F>),
    Binary(Binary<'cx, F>),
    Apply(F::Apply),
    Set(Set<'cx, F>),
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

    pub fn apply_literal(
        ctx: &'cx context::Context<'cx, F>,
        literal: literal::Literal<'cx, F>,
        args: Vec<Self>,
    ) -> Self
    where
        F: TypeFamily<'cx, Literal = literal::Literal<'cx, F>, Apply = Apply<'cx, F>>,
    {
        let function_expr = ctx.alloc_expr(ExprKind::<F>::Literal(literal));

        let args_slice: &[Self] = Box::leak(args.into_boxed_slice());

        let apply_expr = ExprKind::Apply(Apply {
            function: function_expr,
            args: args_slice,
        });

        ctx.alloc_expr(apply_expr)
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
        expr: Self,
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
    pub fn apply(ctx: &'cx context::Context<'cx, F>, function: Self, args: Vec<Self>) -> Self
    where
        F: TypeFamily<'cx, Apply = Apply<'cx, F>>,
    {
        let apply = Apply {
            function,
            args: Box::leak(args.into_boxed_slice()),
        };
        ctx.alloc_expr(ExprKind::Apply(apply))
    }

    #[inline]
    pub fn binary(ctx: &'cx context::Context<'cx, F>, lhs: Self, op: BinOp, rhs: Self) -> Self {
        let binary = Binary { lhs, op, rhs };
        ctx.alloc_expr(ExprKind::Binary(binary))
    }

    #[inline]
    pub fn unary(ctx: &'cx context::Context<'cx, F>, op: UnOp<'cx, F>, expr: Self) -> Self {
        let unary = Unary { op, expr };
        ctx.alloc_expr(ExprKind::Unary(unary))
    }

    #[inline]
    pub fn compare(
        ctx: &'cx context::Context<'cx, F>,
        head: Self,
        tail_with_op: Vec<(CompareOp<'cx, F>, Self)>,
    ) -> Self {
        let compare = Compare {
            head,
            tail_with_op: Box::leak(tail_with_op.into_boxed_slice()),
        };
        ctx.alloc_expr(ExprKind::Compare(compare))
    }

    #[inline]
    pub fn compare_single(
        ctx: &'cx context::Context<'cx, F>,
        lhs: Self,
        op: CompareOp<'cx, F>,
        rhs: Self,
    ) -> Self {
        Self::compare(ctx, lhs, vec![(op, rhs)])
    }

    #[inline]
    pub fn set(
        ctx: &'cx context::Context<'cx, F>,
        lhs: Self,
        colon_eq: token::ColonEq<'cx, F>,
        rhs: Self,
    ) -> Self {
        let set = Set { lhs, colon_eq, rhs };
        ctx.alloc_expr(ExprKind::Set(set))
    }

    #[inline]
    pub fn qualif(ctx: &'cx context::Context<'cx, F>, qualif: F::Qualif) -> Self {
        ctx.alloc_expr(ExprKind::Qualif(qualif))
    }

    #[inline]
    pub fn pre_qualified(
        ctx: &'cx context::Context<'cx, F>,
        qualifs: Vec<F::Qualif>,
        expr: Self,
    ) -> Self
    where
        F: TypeFamily<'cx, PreQualified = PreQualified<'cx, F>>,
    {
        let pre_qualified = PreQualified {
            qualifs: Box::leak(qualifs.into_boxed_slice()),
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
        let segments = Box::leak(vec![ident].into_boxed_slice());
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
/// A qualification for a command.
pub enum Qualif<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    TimeIndicator(Expr<'cx, F>),
    ExecutorComponent(ExecutorComponent<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// An executor component specification.
///
/// # Examples
///
/// - `AOBC` in `MOBC.TL.NOP :20 @AOBC`.
pub struct ExecutorComponent<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub at_token: token::Atmark<'cx, F>,
    pub name: Path<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// An executor component specification.
///
/// # Examples
///
/// - `:20` in `MOBC.TL.NOP :20 @AOBC` or `:20 @AOBC MOBC.TL.NOP`.
pub struct TimeIndicator<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub value: Expr<'cx, F>,
}

pub use literal::*;

use crate::{V1, version::Versioned};

pub mod literal {
    use super::*;

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Literal<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        Array(Array<'cx, F>),
        String(String<'cx, F>),
        Bytes(Bytes<'cx, F>),
        HexBytes(HexBytes<'cx, F>),
        Numeric(F::Numeric),
        OsFilePath(OsFilePath<'cx, F>),
        DateTime(DateTime<'cx, F>),
    }

    impl<'cx, F: TypeFamily<'cx>> Literal<'cx, F> {
        pub fn string(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
            content: &str,
            span: F::Span,
        ) -> Self {
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
        ) -> Self {
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
        ) -> Self {
            let hex_str = ctx.alloc_str(content);
            Literal::HexBytes(HexBytes { raw: hex_str, span })
        }

        pub fn numeric(numeric: F::Numeric) -> Self
        where
            F: TypeFamily<'cx, Numeric = Numeric<'cx>>,
        {
            Literal::Numeric(numeric)
        }

        pub fn os_file_path(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
            content: &str,
            span: F::Span,
        ) -> Self {
            let path_str = ctx.alloc_str(content);
            Literal::OsFilePath(OsFilePath {
                raw: path_str,
                span,
            })
        }

        pub fn date_time(
            ctx: &'cx crate::syntax::v1::context::Context<'cx, F>,
            content: &str,
            span: F::Span,
        ) -> Self {
            let date_str = ctx.alloc_str(content);
            Literal::DateTime(DateTime {
                raw: date_str,
                span,
            })
        }

        pub fn array(
            left_bracket: token::OpenSquare<'cx, F>,
            exprs: &'cx [Expr<'cx, F>],
            right_bracket: token::CloseSquare<'cx, F>,
        ) -> Self {
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
        pub exprs: &'cx [Expr<'cx, F>],
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
    /// A file path.
    ///
    /// The file path is a string that represents the location of a file.
    /// It can be a relative or absolute path.
    pub struct OsFilePath<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        pub raw: &'cx str,
        pub span: F::Span,
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
    pub expr: Expr<'cx, F>,
    pub right_paren: token::CloseParen<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct PreQualified<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub qualifs: &'cx [F::Qualif],
    pub expr: Expr<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Unary<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub op: UnOp<'cx, F>,
    pub expr: Expr<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOp<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    /// Negates an expression.
    Neg(token::Hyphen<'cx, F>),

    /// Create a reference of an expression.
    ///
    /// This is a temporal solution for accepting the old `tlmid!` functionality.
    Ref(token::Ampersand<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Compare<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub head: Expr<'cx, F>,
    pub tail_with_op: &'cx [(CompareOp<'cx, F>, Expr<'cx, F>)],
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CompareOp<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    GreaterEq(token::RightAngleEq<'cx, F>),
    LessEq(token::AngleEq<'cx, F>),
    Greater(token::RightAngle<'cx, F>),
    Less(token::Angle<'cx, F>),
    NotEqual(NotEqualToken<'cx, F>),
    Equal(token::EqualEqual<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NotEqualToken<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    /// `!=`
    BangEqual(token::BangEqual<'cx, F>),
    /// `/=`
    SlashEqual(token::SlashEqual<'cx, F>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Binary<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub lhs: Expr<'cx, F>,
    pub op: BinOp,
    pub rhs: Expr<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinOp {
    /// `a if b` (it means `b implies a`).
    ///
    /// It will be deleted and replaced by `if b then a else ..` in the future.
    If,

    /// `&&`
    And,
    /// `||`
    Or,

    /// `in`
    In,

    /// `*`
    Mul,
    /// `/`
    Div,
    /// `%`
    Mod,
    /// `+`
    Add,
    /// `-`
    Sub,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Apply<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub function: Expr<'cx, F>,
    pub args: &'cx [Expr<'cx, F>],
}

#[derive(Debug, PartialEq, Clone, Copy, OrderSpan)]
pub struct Set<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub lhs: Expr<'cx, F>,
    pub colon_eq: token::ColonEq<'cx, F>,
    pub rhs: Expr<'cx, F>,
}
