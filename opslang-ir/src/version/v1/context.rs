use super::syn;
use super::{
    Apply, Bytes, Comment, DateTime, Expr, HexBytes, IrTypeFamily, Numeric, ResolvedPath, String,
};
use opslang_ty::version::v1::Ty;
use syn::context::Context as AstContext;
use syn::{Block, ExprKind, Row};
use typed_arena::Arena;

/// A context for constructing IR expressions with type information.
///
/// This context wraps an AST context and adds additional allocation arenas
/// for IR-specific types that include type information and resolved names.
#[derive(Default)]
pub struct Context<'cx> {
    /// Underlying AST context for basic allocations
    ast_context: AstContext<'cx, IrTypeFamily>,

    /// Arena for IR-specific comment blocks
    comment_arena: Arena<Comment<'cx>>,

    /// Arena for IR-specific string literals
    string_arena: Arena<String<'cx>>,

    /// Arena for IR-specific bytes literals
    bytes_arena: Arena<Bytes<'cx>>,

    /// Arena for IR-specific hex bytes literals
    hex_bytes_arena: Arena<HexBytes<'cx>>,

    /// Arena for IR-specific datetime literals
    datetime_arena: Arena<DateTime<'cx>>,

    /// Arena for IR-specific numeric literals
    numeric_arena: Arena<Numeric<'cx>>,

    /// Arena for IR-specific apply expressions
    apply_arena: Arena<Apply<'cx>>,

    /// Arena for IR-specific expressions with types
    expr_arena: Arena<Expr<'cx>>,

    /// Arena for resolved path references
    resolved_path_arena: Arena<ResolvedPath<'cx>>,
}

impl<'cx> std::ops::Deref for Context<'cx> {
    type Target = AstContext<'cx, IrTypeFamily>;

    fn deref(&self) -> &Self::Target {
        &self.ast_context
    }
}

impl<'cx> Context<'cx> {
    /// Creates a new IR context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Gets a reference to the underlying AST context.
    pub fn ast_context(&self) -> &AstContext<'cx, IrTypeFamily> {
        &self.ast_context
    }

    /// Allocates a string in the context.
    pub fn alloc_str(&'cx self, string: &str) -> &'cx str {
        self.ast_context.alloc_str(string)
    }

    /// Allocates bytes in the context.
    pub fn alloc_bytes(&'cx self, bytes: &[u8]) -> &'cx [u8] {
        self.ast_context.alloc_bytes(bytes)
    }

    /// Allocates an IR comment block.
    pub fn alloc_comment(&'cx self, comment: Comment<'cx>) -> &'cx Comment<'cx> {
        self.comment_arena.alloc(comment)
    }

    /// Allocates an IR string literal.
    pub fn alloc_string(&'cx self, string: String<'cx>) -> &'cx String<'cx> {
        self.string_arena.alloc(string)
    }

    /// Allocates an IR bytes literal.
    pub fn alloc_bytes_literal(&'cx self, bytes: Bytes<'cx>) -> &'cx Bytes<'cx> {
        self.bytes_arena.alloc(bytes)
    }

    /// Allocates an IR hex bytes literal.
    pub fn alloc_hex_bytes(&'cx self, hex_bytes: HexBytes<'cx>) -> &'cx HexBytes<'cx> {
        self.hex_bytes_arena.alloc(hex_bytes)
    }

    /// Allocates an IR datetime literal.
    pub fn alloc_datetime(&'cx self, datetime: DateTime<'cx>) -> &'cx DateTime<'cx> {
        self.datetime_arena.alloc(datetime)
    }

    /// Allocates an IR numeric literal.
    pub fn alloc_numeric(&'cx self, numeric: Numeric<'cx>) -> &'cx Numeric<'cx> {
        self.numeric_arena.alloc(numeric)
    }

    /// Allocates an IR apply expression.
    pub fn alloc_apply(&'cx self, apply: Apply<'cx>) -> &'cx Apply<'cx> {
        self.apply_arena.alloc(apply)
    }

    /// Allocates an IR expression with type information.
    pub fn alloc_expr(&'cx self, expr: Expr<'cx>) -> &'cx Expr<'cx> {
        self.expr_arena.alloc(expr)
    }

    /// Allocates a resolved path reference.
    pub fn alloc_resolved_path(
        &'cx self,
        resolved_path: ResolvedPath<'cx>,
    ) -> &'cx ResolvedPath<'cx> {
        self.resolved_path_arena.alloc(resolved_path)
    }

    /// Allocates a row using the underlying AST context.
    pub fn alloc_row(&'cx self, row: Row<'cx, IrTypeFamily>) -> &'cx Row<'cx, IrTypeFamily> {
        self.ast_context.alloc_row(row)
    }

    /// Allocates a block using the underlying AST context.
    pub fn alloc_block(
        &'cx self,
        block: Block<'cx, IrTypeFamily>,
    ) -> &'cx Block<'cx, IrTypeFamily> {
        self.ast_context.alloc_block(block)
    }

    /// Creates an IR expression with type information from expression kind and type.
    ///
    /// This method first uses the AST context to allocate the expression kind,
    /// then creates an IR expression with the provided type information.
    pub fn alloc_expr_with_type(
        &'cx self,
        ir_expr_kind: ExprKind<'cx, IrTypeFamily>,
        expected_type: Ty<'cx>,
    ) -> Expr<'cx> {
        let expr_ref = self.ast_context.alloc_expr_mut(ir_expr_kind);
        Expr {
            kind: expr_ref,
            ty: expected_type,
        }
    }
}
