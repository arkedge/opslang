use family::TypeFamily;
use opslang_ast_macros::{OrderSpan, TrivialBridge};

pub mod family;
pub mod loc;
pub mod token;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BytePos(pub u32);
pub type Position = BytePos;

#[derive(Debug, PartialEq, Clone, Copy)]
/// Default value for each types in this crate, to allow this crate define an AST.
///
/// This type does not take any lifetime parameters because [`TypeFamily`] trait has them.
pub struct DefaultTypeFamily;

impl<'cx> TypeFamily<'cx> for DefaultTypeFamily {
    type Comment = &'cx Comment<'cx>;
    type CommentSpan = Span;
    type Row = &'cx Row<'cx>;
    type RowContent = StatementKind<'cx>;
    type Block = &'cx Block<'cx>;
    type ScopeItem = ScopeItem<'cx>;
    type ReturnStmt = ReturnStmt;

    type Ident = Ident<'cx>;
    type Path = Path<'cx>;

    type SemiToken = token::Semi;
    type BreakToken = token::Break;
    type LetToken = token::Let;
    type EqToken = token::Eq;
    type ColonEqToken = token::ColonEq;

    type Qualif = Qualif<'cx>;
    type PreQualified = PreQualified<'cx>;
    type Parened = Parened<'cx>;
    type Literal = Literal<'cx>;
    type Numeric = Numeric<'cx>;
    type Apply = Apply<'cx>;

    type UnOp = UnOp;
    type CompareOp = CompareOp;
    type BinOp = BinOp;
}

#[derive(Debug, PartialEq, Clone, Copy, TrivialBridge)]
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
    pub breaks: Option<F::BreakToken>,
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
    pub span: F::CommentSpan,
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
    pub left_brace: token::OpenBrace,
    pub scope: Scope<'cx, F>,
    pub right_brace: token::CloseBrace,
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
    pub let_token: F::LetToken,
    pub variable: F::Ident,
    pub eq: F::EqToken,
    pub rhs: Expr<'cx, F>,
    pub semi: F::SemiToken,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A statement kind.
pub struct ExprStatement<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub expr: Expr<'cx, F>,
    pub semi: F::SemiToken,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A `return` statement.
///
/// # Examples
///
/// ```ops
/// return;
/// ```
pub struct ReturnStmt {
    pub return_token: token::Return,
    pub semi: token::Semi,
}

pub type OwnedExpr<'cx, F = DefaultTypeFamily> = ExprKind<'cx, F>;
pub type Expr<'cx, F = DefaultTypeFamily> = &'cx OwnedExpr<'cx, F>;

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

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Path<'cx> {
    pub raw: &'cx str,
    pub segments: &'cx [Ident<'cx>],
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Ident<'cx> {
    pub raw: &'cx str,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A qualification for a command.
pub enum Qualif<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    TimeIndicator(Expr<'cx, F>),
    ExecutorComponent(ExecutorComponent<'cx>),
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// An executor component specification.
///
/// # Examples
///
/// - `AOBC` in `MOBC.TL.NOP :20 @AOBC`.
pub struct ExecutorComponent<'cx> {
    pub at_token: token::Atmark,
    pub name: Path<'cx>,
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
        String(String<'cx>),
        Bytes(Bytes<'cx>),
        HexBytes(HexBytes<'cx>),
        Numeric(F::Numeric),
        OsFilePath(OsFilePath<'cx>),
        DateTime(DateTime<'cx>),
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct Array<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
        pub left_bracket: token::OpenSquare,
        pub exprs: &'cx [Expr<'cx, F>],
        pub right_bracket: token::CloseSquare,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct String<'cx> {
        pub raw: &'cx str,
        pub span: Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct Bytes<'cx> {
        pub raw: &'cx str,
        pub span: Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct HexBytes<'cx> {
        pub raw: &'cx str,
        pub span: Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct Numeric<'cx> {
        /// The raw string representation of the numeric value, without any prefix or suffix.
        pub raw: &'cx str,

        pub kind: NumericKind,
        pub suffix: Option<NumericSuffix<'cx>>,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum NumericKind {
        Integer(IntegerPrefix),
        Float,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    /// Suffix of numeral value. Allows any ident at this point.
    pub struct NumericSuffix<'cx>(pub Ident<'cx>);

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
    pub struct OsFilePath<'cx> {
        pub raw: &'cx str,
        pub span: Span,
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    /// A date-time value.
    pub struct DateTime<'cx> {
        pub raw: &'cx str,
        pub span: Span,
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Parened<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub left_paren: token::OpenParen,
    pub expr: Expr<'cx, F>,
    pub right_paren: token::CloseParen,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct PreQualified<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub qualifs: &'cx [F::Qualif],
    pub expr: Expr<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Unary<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub op: F::UnOp,
    pub expr: Expr<'cx, F>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnOp {
    /// Negates an expression.
    Neg(token::Hyphen),

    /// Create a reference of an expression.
    ///
    /// This is a temporal solution for accepting the old `tlmid!` functionality.
    Ref(token::Ampersand),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Compare<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub head: Expr<'cx, F>,
    pub tail_with_op: &'cx [(F::CompareOp, Expr<'cx, F>)],
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CompareOp {
    GreaterEq(token::RightAngleEq),
    LessEq(token::AngleEq),
    Greater(token::RightAngle),
    Less(token::Angle),
    NotEqual(NotEqualToken),
    Equal(token::EqualEqual),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NotEqualToken {
    /// `!=`
    BangEqual(token::BangEqual),
    /// `/=`
    SlashEqual(token::SlashEqual),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Binary<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    pub lhs: Expr<'cx, F>,
    pub op: F::BinOp,
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
    pub colon_eq: F::ColonEqToken,
    pub rhs: Expr<'cx, F>,
}
