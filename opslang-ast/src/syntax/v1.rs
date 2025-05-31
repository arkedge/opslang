use interner::Interner;
use opslang_ast_macros::{OrderSpan, TrivialBridge};

pub mod interner;
pub mod loc;
pub mod token;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BytePos(pub u32);
pub type Position = BytePos;

#[derive(Debug, PartialEq, Clone, Copy)]
/// Default value for each types in this crate, to allow this crate define an AST.
///
/// This type does not take any lifetime parameters because [`Interner`] trait has them.
pub struct DefaultInterner;

impl<'cx> Interner<'cx> for DefaultInterner {
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
pub struct Program<'cx, I: Interner<'cx> = DefaultInterner> {
    pub content: Scope<'cx, I>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// Sequence of statements.
pub struct Scope<'cx, I: Interner<'cx> = DefaultInterner> {
    pub items: &'cx [I::ScopeItem],
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A scope item can be a single statement or a block of statements, or a comment.
pub enum ScopeItem<'cx, I: Interner<'cx> = DefaultInterner> {
    Row(I::Row),
    Block(I::Block),
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A single row of program with optional comments and breaks.
pub struct Row<'cx, I: Interner<'cx> = DefaultInterner> {
    pub breaks: Option<I::BreakToken>,
    pub content: Option<I::RowContent>,
    pub comment: Option<I::Comment>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A comment in a program.
pub struct Comment<'cx, I: Interner<'cx> = DefaultInterner> {
    pub content: &'cx str,
    pub span: I::CommentSpan,
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
pub struct Block<'cx, I: Interner<'cx> = DefaultInterner> {
    pub left_brace: token::OpenBrace,
    pub scope: Scope<'cx, I>,
    pub right_brace: token::CloseBrace,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A statement kind.
pub enum StatementKind<'cx, I: Interner<'cx> = DefaultInterner> {
    Let(Let<'cx, I>),
    Expr(ExprStatement<'cx, I>),
    Return(I::ReturnStmt),
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A let statement.
///
/// # Examples
///
/// ```ops
/// let d = 1s
/// ```
pub struct Let<'cx, I: Interner<'cx> = DefaultInterner> {
    pub let_token: I::LetToken,
    pub variable: I::Ident,
    pub eq: I::EqToken,
    pub rhs: Expr<'cx, I>,
    pub semi: I::SemiToken,
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// A statement kind.
pub struct ExprStatement<'cx, I: Interner<'cx> = DefaultInterner> {
    pub expr: Expr<'cx, I>,
    pub semi: I::SemiToken,
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

pub type OwnedExpr<'cx, I = DefaultInterner> = ExprKind<'cx, I>;
pub type Expr<'cx, I = DefaultInterner> = &'cx OwnedExpr<'cx, I>;

#[derive(Debug, PartialEq, Clone, Copy)]
/// An expression.
pub enum ExprKind<'cx, I: Interner<'cx> = DefaultInterner> {
    Variable(I::Path),
    Literal(I::Literal),
    Parened(I::Parened),
    Qualif(I::Qualif),
    PreQualified(I::PreQualified),
    Unary(Unary<'cx, I>),
    Compare(Compare<'cx, I>),
    Binary(Binary<'cx, I>),
    Apply(I::Apply),
    Set(Set<'cx, I>),
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
pub enum Qualif<'cx, I: Interner<'cx> = DefaultInterner> {
    TimeIndicator(Expr<'cx, I>),
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
pub struct TimeIndicator<'cx, I: Interner<'cx> = DefaultInterner> {
    pub value: Expr<'cx, I>,
}

pub use literal::*;

pub mod literal {
    use super::*;

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Literal<'cx, I: Interner<'cx> = DefaultInterner> {
        Array(Array<'cx, I>),
        String(String<'cx>),
        Bytes(Bytes<'cx>),
        HexBytes(HexBytes<'cx>),
        Numeric(I::Numeric),
        OsFilePath(OsFilePath<'cx>),
        DateTime(DateTime<'cx>),
    }

    #[derive(Debug, PartialEq, Clone, Copy)]
    pub struct Array<'cx, I: Interner<'cx> = DefaultInterner> {
        pub left_bracket: token::OpenSquare,
        pub exprs: &'cx [Expr<'cx, I>],
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
pub struct Parened<'cx, I: Interner<'cx> = DefaultInterner> {
    pub left_paren: token::OpenParen,
    pub expr: Expr<'cx, I>,
    pub right_paren: token::CloseParen,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct PreQualified<'cx, I: Interner<'cx> = DefaultInterner> {
    pub qualifs: &'cx [I::Qualif],
    pub expr: Expr<'cx, I>,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Unary<'cx, I: Interner<'cx> = DefaultInterner> {
    pub op: I::UnOp,
    pub expr: Expr<'cx, I>,
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
pub struct Compare<'cx, I: Interner<'cx> = DefaultInterner> {
    pub head: Expr<'cx, I>,
    pub tail_with_op: &'cx [(I::CompareOp, Expr<'cx, I>)],
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
pub struct Binary<'cx, I: Interner<'cx> = DefaultInterner> {
    pub lhs: Expr<'cx, I>,
    pub op: I::BinOp,
    pub rhs: Expr<'cx, I>,
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
pub struct Apply<'cx, I: Interner<'cx> = DefaultInterner> {
    pub function: Expr<'cx, I>,
    pub args: &'cx [Expr<'cx, I>],
}

#[derive(Debug, PartialEq, Clone, Copy, OrderSpan)]
pub struct Set<'cx, I: Interner<'cx> = DefaultInterner> {
    pub lhs: Expr<'cx, I>,
    pub colon_eq: I::ColonEqToken,
    pub rhs: Expr<'cx, I>,
}
