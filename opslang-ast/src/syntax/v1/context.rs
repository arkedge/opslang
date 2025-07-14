use super::{Block, Comment, ExprKind, Row};
use typed_arena::Arena;

#[derive(Default)]
/// A context for constructing expressions.
///
/// This type exists to hold the contents of reference types, which is introduced:
/// - to reduce type size and
/// - to provide uniform lifetimes for mutually recursive types.
pub struct Context<'cx> {
    str_arena: Arena<u8>,
    expr_arena: Arena<ExprKind<'cx>>,
    row_arena: Arena<Row<'cx>>,
    block_arena: Arena<Block<'cx>>,
    comment_arena: Arena<Comment<'cx>>,
}

impl<'cx> Context<'cx> {
    pub fn alloc_str<'any>(&'cx self, string: &'any str) -> &'cx str {
        if string.is_empty() {
            return "";
        }
        self.str_arena.alloc_str(string)
    }
    pub fn alloc_bytes<'any>(&'cx self, bytes: &'any [u8]) -> &'cx [u8] {
        self.str_arena.alloc_extend(bytes.iter().copied())
    }

    pub fn alloc_expr(&'cx self, expr: ExprKind<'cx>) -> &'cx ExprKind<'cx> {
        self.expr_arena.alloc(expr)
    }

    pub fn alloc_row(&'cx self, row: Row<'cx>) -> &'cx Row<'cx> {
        self.row_arena.alloc(row)
    }

    pub fn alloc_block(&'cx self, block: Block<'cx>) -> &'cx Block<'cx> {
        self.block_arena.alloc(block)
    }

    pub fn alloc_comment(&'cx self, comment: Comment<'cx>) -> &'cx Comment<'cx> {
        self.comment_arena.alloc(comment)
    }

    pub fn new() -> Self {
        Self::default()
    }
}
