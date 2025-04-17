use opslang_ast::v1::ExprKind;
use typed_arena::Arena;

pub struct Context<'cx> {
    expr_arena: Arena<ExprKind<'cx>>,
}
