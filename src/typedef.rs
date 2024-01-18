//! type definition of AST.

use std::ops::Range;

#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Range<usize>,
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Debug, PartialEq)]
pub struct Row {
    pub breaks: Option<()>,
    pub content: Option<ReservedControl>,
    pub comment_trailing: Option<Comment>,
}
pub type SRow = Spanned<Row>;

#[derive(Debug, PartialEq)]
pub struct Comment(pub String);

/// reserved controls
#[derive(Debug, PartialEq)]
pub enum ReservedControl {
    Call(Call),
    WaitSec(WaitSec),
    WaitUntil(WaitUntil),
    CheckValue(CheckValue),
    Command(Command),
    Let(Let),
    Get(Get),
}

#[derive(Debug, PartialEq)]
pub struct VariablePath {
    pub raw: String,
}

#[derive(Debug, PartialEq)]
pub struct FilePath {
    pub full_name: String,
}

#[derive(Debug, PartialEq)]
pub struct CheckValue {
    pub condition: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Command {
    pub destinations: Vec<Destination>,
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Destination {
    pub component: String,
    pub exec_method: String,
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub path: FilePath,
}

#[derive(Debug, PartialEq)]
pub struct WaitSec {
    pub sec: Expr,
}

#[derive(Debug, PartialEq)]
pub struct WaitUntil {
    pub condition: Expr,
}

#[derive(Debug, PartialEq)]
pub struct WaitInc {
    pub condition: Expr,
}

#[derive(Debug, PartialEq)]
pub enum CompareOp {
    BinOp(CompareBinOp),
    In(CompareIn),
}

#[derive(Debug, PartialEq)]
pub struct CompareBinOp {
    pub method: CompareBinOpKind,
    pub rhs: Expr,
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
pub struct CompareIn {
    pub lo: Expr,
    pub hi: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Ident {
    pub raw: String,
}

#[derive(Debug, PartialEq)]
pub struct Let {
    pub variable: Ident,
    pub rhs: Expr,
}

#[derive(Debug, PartialEq)]
pub struct Get {
    pub variable: VariablePath,
}

/// an expression.
///
/// to implementer: you can use stack machine to express the evaluation of this tree structure.
#[derive(Debug, PartialEq)]
pub enum Expr {
    Variable(VariablePath),
    Literal(Literal),
    UnOp(UnOpKind, Box<Self>),
    BinOp(BinOpKind, Box<Self>, Box<Self>),
    FunCall(Box<Self>, Vec<Self>),
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Array(Vec<Expr>),
    String(String),
    Numeric(Numeric, Option<NumericSuffix>),
}

#[derive(Debug, PartialEq)]
pub enum Numeric {
    Integer(String, IntegerPrefix),
    Float(String),
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
