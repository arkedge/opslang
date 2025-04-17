use opslang_ast::v1::{ExecutorComponent, Expr, ReceiverComponent};

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
    pub receiver_component: Option<ReceiverComponent<'cx>>,
    pub time_indicator: Option<Expr<'cx>>,
    pub executor_component: Option<ExecutorComponent<'cx>>,
}
