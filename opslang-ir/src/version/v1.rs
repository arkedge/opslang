use opslang_ast::v1::{self, ExecutorComponent, Expr};

#[derive(Debug, PartialEq)]
/// A comment in the code.
///
/// This is a concatenation of adjacent comments.
pub struct Comment<'cx> {
    pub content: &'cx str,
}

#[derive(Debug, PartialEq)]
pub struct Scope<'cx> {
    pub content: &'cx [ScopeContent<'cx>],
}

#[derive(Debug, PartialEq)]
/// A scope content can be a single statement or a block of statements.
pub enum ScopeContent<'cx> {
    Statement(&'cx Statement<'cx>),
    Block(&'cx Block<'cx>),
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

    /// The corresponding element in the AST.
    pub syn: &'cx v1::Block<'cx>,
}

#[derive(Debug, PartialEq)]
/// A statement with optional comments and breaks.
pub struct Statement<'cx> {
    pub kind: StatementKind<'cx>,

    /// The corresponding element in the AST.
    pub syn: &'cx v1::Row<'cx>,
}

#[derive(Debug, PartialEq)]
/// A statement kind.
pub enum StatementKind<'cx> {
    Let(Let<'cx>),
    Expr(Expr<'cx>),
    Return,
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
    pub variable: v1::Ident<'cx>,
    pub rhs: Expr<'cx>,
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
/// ```ignore (illustrative)
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
    pub destination: DestinationSpec<'cx>,
    pub name: &'cx str,
    pub args: &'cx [Expr<'cx>],
}

#[derive(Debug, PartialEq)]
/// A specification of a destination for a command.
///
/// # Examples
///
/// - `@RT.MOBC` in `@RT.MOBC NOP`.
/// - `@TL.MOBC 20` in `@TL.MOBC 20: NOP`.
/// - `@TL.MOBC 20: @@AOBC NOP` in `@TL.MOBC 20: @@AOBC NOP`.
pub struct DestinationSpec<'cx> {
    pub time_indicator: Option<Expr<'cx>>,
    pub executor_component: Option<ExecutorComponent<'cx>>,
}
