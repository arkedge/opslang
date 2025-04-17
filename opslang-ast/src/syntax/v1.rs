use chrono::{DateTime, Utc};
use std::ops::Range;

pub trait Span {
    fn span(&self) -> Range<usize>;
}

#[derive(Debug, PartialEq)]
/// A whole program. The program is a sequence of statements.
pub struct Program<'cx> {
    pub content: Scope<'cx>,
}

#[derive(Debug, PartialEq)]
/// A comment in the code.
///
/// Comments are NOT ignored by the parser.
pub struct Comment<'cx> {
    pub content: &'cx str,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq)]
/// A semicolon in the code.
pub struct Semi {
    pub start: usize,
}

#[derive(Debug, PartialEq)]
pub struct Scope<'cx> {
    pub content: &'cx [ScopeContent<'cx>],
}

#[derive(Debug, PartialEq)]
/// A block of statements with optional comments and a default receiver component. The block can also have a delay.
///
/// # Examples
///
/// ```ops
/// @RT.MOBC delay=0.5s {
///     NOP
///     NOP
/// }
/// ```
pub struct Block<'cx> {
    pub scope: Scope<'cx>,

    /// Comment *before* the block beginning.
    pub comment_leading: Option<&'cx Comment<'cx>>,

    /// Comment after the block ending.
    pub comment_trailing: Option<&'cx Comment<'cx>>,

    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq)]
/// A scope content can be a single statement or a block of statements.
pub enum ScopeContent<'cx> {
    Statement(&'cx Statement<'cx>),
    Block(&'cx Block<'cx>),
}

#[derive(Debug, PartialEq)]
/// A statement with optional comments and breaks.
pub struct Statement<'cx> {
    pub breaks: Option<()>,
    pub content: Option<&'cx StatementKind<'cx>>,
    pub comment_leading: Option<&'cx Comment<'cx>>,
    pub comment_trailing: Option<&'cx Comment<'cx>>,
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq)]
/// A statement kind.
pub enum StatementKind<'cx> {
    Let(Let<'cx>),
    Expr(Expr<'cx>, Semi),
    Return,
}

#[derive(Debug, PartialEq)]
/// Reserved control statements.
pub enum Control<'cx> {
    Call(Call<'cx>),
    Wait(Wait<'cx>),
    Assert(Assert<'cx>),
    AssertEq(AssertEq<'cx>),
    SendCommand(SendCommand<'cx>),
    Print(Print<'cx>),
    Set(Set<'cx>),
}

#[derive(Debug, PartialEq)]
pub struct Path<'cx> {
    pub raw: &'cx str,
    pub segments: &'cx [Ident<'cx>],
}

#[derive(Debug, PartialEq)]
pub struct Ident<'cx>(pub &'cx str);

#[derive(Debug, PartialEq)]
/// A file path.
///
/// The file path is a string that represents the location of a file.
/// It can be a relative or absolute path.
pub struct FilePath<'cx> {
    pub full_name: &'cx str,
}

#[derive(Debug, PartialEq)]
/// An assertion that a condition is true.
///
/// # Examples
///
/// ```ops
/// assert 2 == 2
/// ```
pub struct Assert<'cx> {
    pub condition: Expr<'cx>,
}

#[derive(Debug, PartialEq)]
/// An assertion that two expressions are equal within an optional tolerance.
///
/// # Examples
///
/// ```ops
/// assert_eq 2 2
/// ```
///
/// ```ops
/// assert_approx_eq 1.0 1.0 0.001
/// ```
pub struct AssertEq<'cx> {
    pub left: Expr<'cx>,
    pub right: Expr<'cx>,
    pub tolerance: Option<Expr<'cx>>,
}

#[derive(Debug, PartialEq)]
/// A command to be sent to a component.
///
/// # Examples
///
/// ```ops
/// @RT.MOBC NOP
/// ```
///
/// parsed as:
/// ```no_run
/// SendCommand {
///     destination: DestinationSpec {
///         receiver_component: Some(ReceiverComponent {
///             name: "MOBC",
///             exec_method: "RT",
///         }),
///         time_indicator: None,
///         executor_component: None,
///     },
///     name: "NOP",
///     args: vec![],
/// }
/// ```
pub struct SendCommand<'cx> {
    pub name: &'cx str,
    pub args: &'cx [Expr<'cx>],
}

#[derive(Debug, PartialEq)]
/// A monoid act for a command.
pub enum MonoidAct<'cx> {
    ReceiverComponent(ReceiverComponent<'cx>),
    TimeIndicator(Expr<'cx>),
    ExecutorComponent(ExecutorComponent<'cx>),
}

#[derive(Debug, PartialEq)]
/// A receiver component specification.
///
/// # Examples
///
/// - `RT.MOBC` in `@RT.MOBC NOP`.
/// - `TL.MOBC` in `@TL.MOBC 20: NOP`.
/// - `TL.MOBC` in `@TL.MOBC 20: @@AOBC NOP`.
pub struct ReceiverComponent<'cx> {
    pub name: &'cx str,
    pub exec_method: &'cx str,
}

#[derive(Debug, PartialEq)]
/// An executor component specification.
///
/// # Examples
///
/// - `AOBC` in `@TL.MOBC 20: @@AOBC NOP`.
pub struct ExecutorComponent<'cx> {
    pub name: &'cx str,
}

#[derive(Debug, PartialEq)]
/// A file call.
pub struct Call<'cx> {
    pub path: FilePath<'cx>,
}

#[derive(Debug, PartialEq)]
/// A wait statement.
pub struct Wait<'cx> {
    pub condition: Expr<'cx>,
}

#[derive(Debug, PartialEq)]
pub struct WaitInc<'cx> {
    pub condition: Expr<'cx>,
}

#[derive(Debug, PartialEq)]
/// A let statement.
///
/// # Examples
///
/// ```ops
/// let d = 1s
/// ```
pub struct Let<'cx> {
    pub variable: Ident<'cx>,
    pub rhs: Expr<'cx>,
}

#[derive(Debug, PartialEq)]
/// A print statement.
pub struct Print<'cx> {
    pub arg: Expr<'cx>,
}

#[derive(Debug, PartialEq)]
pub struct Set<'cx> {
    pub name: Path<'cx>,
    pub expr: Expr<'cx>,
}

#[derive(Debug, PartialEq)]
/// An expression.
pub enum ExprKind<'cx> {
    MonoidAct(MonoidAct<'cx>),
    Control(Control<'cx>),
    Variable(Path<'cx>),
    Literal(Literal<'cx>),
    UnOp(UnOpKind, Expr<'cx>),
    BinOp(BinOpKind, Expr<'cx>, Expr<'cx>),
    Apply(Expr<'cx>, &'cx [Expr<'cx>]),
}

pub type Expr<'cx> = &'cx ExprKind<'cx>;

#[derive(Debug, PartialEq)]
pub enum Literal<'cx> {
    Array(&'cx [Expr<'cx>]),
    String(&'cx str),
    Numeric {
        raw: &'cx str,
        value: Numeric,
        suffix: Option<NumericSuffix>,
    },
    DateTime(DateTime<Utc>),
    TlmId(&'cx str),
    Bytes(&'cx [u8]),
}

#[derive(Debug, PartialEq)]
pub enum Numeric {
    Integer(i64, IntegerPrefix),
    Float(f64),
}

#[derive(Debug, PartialEq)]
pub enum NumericSuffix {
    Second,
}

#[derive(Debug, PartialEq)]
pub enum IntegerPrefix {
    Hexadecimal,
    Decimal,
    Octal,
    Binary,
}

#[derive(Debug, PartialEq)]
pub enum UnOpKind {
    Neg,
}

#[derive(Debug, PartialEq)]
pub enum CompareBinOpKind {
    GreaterEq,
    LessEq,
    Greater,
    Less,
    NotEqual,
    Equal,
}

#[derive(Debug, PartialEq)]
pub enum BinOpKind {
    /// Compare operators.
    Compare(CompareBinOpKind),

    /// `a if b` (it means `b implies a`).
    If,

    And,
    Or,

    In,

    Mul,
    Div,
    Mod,
    Add,
    Sub,
}
