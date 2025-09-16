use crate::SelectItem;

use super::{
    Block, Comment, CompareOp, DefaultTypeFamily, Expr, ExprKind, ExprMut, Parameter, Row,
    ScopeItem, ToplevelItem, family::TypeFamily,
};
use typed_arena::Arena;

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

    // Slice arenas for different types
    toplevel_slice_arena: Arena<ToplevelItem<'cx, F>>,
    parameter_slice_arena: Arena<Parameter<'cx, F>>,
    scope_item_slice_arena: Arena<ScopeItem<'cx, F>>,
    expr_slice_arena: Arena<F::Expr>,
    ident_slice_arena: Arena<F::Ident>,
    select_item_slice_arena: Arena<SelectItem<'cx, F>>,
    qualif_slice_arena: Arena<F::Qualif>,
    compare_op_expr_tuple_slice_arena: Arena<(CompareOp<'cx, F>, F::Expr)>,
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

    pub fn alloc_expr_mut(&'cx self, expr: ExprKind<'cx, F>) -> ExprMut<'cx, F> {
        ExprMut(self.expr_arena.alloc(expr), super::sealed::Sealed)
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

    // Slice allocation methods
    pub fn alloc_toplevel_item_slice(
        &'cx self,
        items: impl IntoIterator<Item = ToplevelItem<'cx, F>>,
    ) -> &'cx [ToplevelItem<'cx, F>] {
        self.toplevel_slice_arena.alloc_extend(items)
    }

    pub fn alloc_parameter_slice(
        &'cx self,
        parameters: impl IntoIterator<Item = Parameter<'cx, F>>,
    ) -> &'cx [Parameter<'cx, F>] {
        self.parameter_slice_arena.alloc_extend(parameters)
    }

    pub fn alloc_scope_item_slice(
        &'cx self,
        items: impl IntoIterator<Item = ScopeItem<'cx, F>>,
    ) -> &'cx [ScopeItem<'cx, F>] {
        self.scope_item_slice_arena.alloc_extend(items)
    }

    pub fn alloc_expr_slice(&'cx self, exprs: impl IntoIterator<Item = F::Expr>) -> &'cx [F::Expr] {
        self.expr_slice_arena.alloc_extend(exprs)
    }

    pub fn alloc_ident_slice(
        &'cx self,
        idents: impl IntoIterator<Item = F::Ident>,
    ) -> &'cx [F::Ident] {
        self.ident_slice_arena.alloc_extend(idents)
    }

    pub fn alloc_select_item_slice(
        &'cx self,
        items: impl IntoIterator<Item = SelectItem<'cx, F>>,
    ) -> &'cx [SelectItem<'cx, F>] {
        self.select_item_slice_arena.alloc_extend(items)
    }

    pub fn alloc_qualif_slice(
        &'cx self,
        qualifs: impl IntoIterator<Item = F::Qualif>,
    ) -> &'cx [F::Qualif] {
        self.qualif_slice_arena.alloc_extend(qualifs)
    }

    pub fn alloc_compare_op_expr_tuple_slice(
        &'cx self,
        tuples: impl IntoIterator<Item = (CompareOp<'cx, F>, F::Expr)>,
    ) -> &'cx [(CompareOp<'cx, F>, F::Expr)] {
        self.compare_op_expr_tuple_slice_arena.alloc_extend(tuples)
    }

    pub fn new() -> Self {
        Self {
            str_arena: Arena::new(),
            expr_arena: Arena::new(),
            row_arena: Arena::new(),
            block_arena: Arena::new(),
            comment_arena: Arena::new(),
            toplevel_slice_arena: Arena::new(),
            parameter_slice_arena: Arena::new(),
            scope_item_slice_arena: Arena::new(),
            expr_slice_arena: Arena::new(),
            ident_slice_arena: Arena::new(),
            select_item_slice_arena: Arena::new(),
            qualif_slice_arena: Arena::new(),
            compare_op_expr_tuple_slice_arena: Arena::new(),
        }
    }
}

impl<'cx, F: TypeFamily<'cx>> Default for Context<'cx, F> {
    fn default() -> Self {
        Self::new()
    }
}
