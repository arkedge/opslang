use opslang_ast::v1::ExprKind;
use typed_arena::Arena;

/// A context for parsing expressions.
///
/// This type exists to hold the contents of reference types, which is introduced:
/// - to reduce type size and
/// - to provide uniform lifetimes for mutually recursive types.
pub struct Context<'cx> {
    str_arena: Arena<u8>,
    expr_arena: Arena<ExprKind<'cx>>,
}

impl<'cx> Context<'cx> {
    pub fn alloc_str<'any>(&'cx self, string: &'any str) -> &'cx str {
        if string.is_empty() {
            return "";
        }
        self.str_arena.alloc_str(string)
    }

    pub fn alloc_expr(&'cx self, expr: ExprKind<'cx>) -> &'cx ExprKind<'cx> {
        self.expr_arena.alloc(expr)
    }
}
