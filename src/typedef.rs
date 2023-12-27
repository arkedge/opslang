//! type definition of AST.

#[derive(Debug, PartialEq)]
pub struct Row<'a> {
    pub breaks: Option<()>,
    pub content: Option<ReservedControl<'a>>,
    pub comment_trailing: Option<Comment<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Comment<'a>(pub &'a str);

/// reserved controls
#[derive(Debug, PartialEq)]
pub enum ReservedControl<'a> {
    Call(Call<'a>),
    WaitSec(WaitSec<'a>),
    WaitUntil(WaitUntil<'a>),
    CheckValue(CheckValue<'a>),
    Command(Command<'a>),
    Let(Let<'a>),
    Get(Get<'a>),
}

#[derive(Debug, PartialEq)]
pub struct VariablePath<'a> {
    pub raw: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct FilePath<'a> {
    pub full_name: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct CheckValue<'a> {
    pub condition: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Command<'a> {
    pub destinations: Vec<Destination<'a>>,
    pub name: &'a str,
    pub args: Vec<Expr<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Destination<'a> {
    pub component: &'a str,
    pub exec_method: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct Call<'a> {
    pub path: FilePath<'a>,
}

#[derive(Debug, PartialEq)]
pub struct WaitSec<'a> {
    pub sec: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct WaitUntil<'a> {
    pub condition: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct WaitInc<'a> {
    pub condition: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub enum CompareOp<'a> {
    BinOp(CompareBinOp<'a>),
    In(CompareIn<'a>),
}

#[derive(Debug, PartialEq)]
pub struct CompareBinOp<'a> {
    pub method: CompareBinOpKind,
    pub rhs: Expr<'a>,
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
pub struct CompareIn<'a> {
    pub lo: Expr<'a>,
    pub hi: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Ident<'a> {
    pub raw: &'a str,
}

#[derive(Debug, PartialEq)]
pub struct Let<'a> {
    pub variable: Ident<'a>,
    pub rhs: Expr<'a>,
}

#[derive(Debug, PartialEq)]
pub struct Get<'a> {
    pub variable: VariablePath<'a>,
}

/// an expression.
///
/// to implementer: you can use stack machine to express the evaluation of this tree structure.
#[derive(Debug, PartialEq)]
pub enum Expr<'a> {
    Variable(VariablePath<'a>),
    Literal(Literal<'a>),
    UnOp(UnOpKind, Box<Self>),
    BinOp(BinOpKind, Box<Self>, Box<Self>),
    FunCall(Box<Self>, Vec<Self>),
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    Array(Vec<Expr<'a>>),
    String(&'a str),
    Numeric(Numeric<'a>, Option<NumericSuffix>),
}

#[derive(Debug, PartialEq)]
pub enum Numeric<'a> {
    Integer(&'a str, IntegerPrefix),
    Float(&'a str),
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
pub enum BinOpKind {
    Compare(CompareBinOpKind),
    /// `a if b` means `b implies a`.
    /// `a if b` will be represented as `Expr::BinOp(BinOpKind::If, a, b)`
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
