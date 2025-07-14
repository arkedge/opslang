use super::{Block, Comment, DefaultTypeFamily, Expr, ExprKind, Row, family::TypeFamily};
use typed_arena::Arena;

#[derive(Default)]
/// A context for constructing expressions.
///
/// This type exists to hold the contents of reference types, which is introduced:
/// - to reduce type size and
/// - to provide uniform lifetimes for mutually recursive types.
pub struct Context<'cx, F: TypeFamily<'cx> = DefaultTypeFamily> {
    str_arena: Arena<u8>,
    expr_arena: Arena<ExprKind<'cx, F>>,
    row_arena: Arena<Row<'cx, F>>,
    block_arena: Arena<Block<'cx, F>>,
    comment_arena: Arena<Comment<'cx, F>>,
}

impl<'cx, F: TypeFamily<'cx>> Context<'cx, F> {
    pub fn alloc_str<'any>(&'cx self, string: &'any str) -> &'cx str {
        if string.is_empty() {
            return "";
        }
        self.str_arena.alloc_str(string)
    }
    pub fn alloc_bytes<'any>(&'cx self, bytes: &'any [u8]) -> &'cx [u8] {
        self.str_arena.alloc_extend(bytes.iter().copied())
    }

    pub fn alloc_expr(&'cx self, expr: ExprKind<'cx, F>) -> Expr<'cx, F> {
        Expr(self.expr_arena.alloc(expr), super::sealed::Sealed)
    }

    pub fn alloc_row(&'cx self, row: Row<'cx, F>) -> &'cx Row<'cx, F> {
        self.row_arena.alloc(row)
    }

    pub fn alloc_block(&'cx self, block: Block<'cx, F>) -> &'cx Block<'cx, F> {
        self.block_arena.alloc(block)
    }

    pub fn alloc_comment(&'cx self, comment: Comment<'cx, F>) -> &'cx Comment<'cx, F> {
        self.comment_arena.alloc(comment)
    }

    pub fn new() -> Self {
        Self::default()
    }
}
