use anyhow::anyhow;
use chrono::Utc;
use opslang_ast::v1::token::IntoToken;
use opslang_ast::v1::{self as ast};
use opslang_ir::version::v1::{self as ir};

use ir::{IrTypeFamily, Typed};
use opslang_ty::version::v1::{
    FloatTy, Ident, InferTy, IntTy, Module, ModuleItem, ModuleLoader, Ty, TyKind, TyVid,
    TypingContext,
};
use opslang_visitor::VisitorMut;
use std::collections::HashMap;

type Result<T, E = anyhow::Error> = std::result::Result<T, E>;

mod hm;
use hm::Substitution;
mod environment;
use environment::Environment;
mod lower;
mod typeck_apply;
mod typeck_constant;
mod typeck_expr;
mod typeck_function;
mod typeck_literal;

/// Creates the builtin module containing all primitive types.
///
/// The builtin module provides access to fundamental types like integers, strings,
/// and other primitives that are available in all contexts without explicit imports.
pub fn create_builtin_module<'cx>(cx: &'cx TypingContext<'cx>) -> &'cx Module<'cx> {
    let mut builtin = Module::new(cx.alloc_toplevel_ident("builtin"));

    // Add all builtin primitive types
    builtin.add_type(cx.alloc_toplevel_ident("i8"), Ty::mk_i8(cx));
    builtin.add_type(cx.alloc_toplevel_ident("i16"), Ty::mk_i16(cx));
    builtin.add_type(cx.alloc_toplevel_ident("i32"), Ty::mk_i32(cx));
    builtin.add_type(cx.alloc_toplevel_ident("i64"), Ty::mk_i64(cx));

    builtin.add_type(cx.alloc_toplevel_ident("u8"), Ty::mk_u8(cx));
    builtin.add_type(cx.alloc_toplevel_ident("u16"), Ty::mk_u16(cx));
    builtin.add_type(cx.alloc_toplevel_ident("u32"), Ty::mk_u32(cx));
    builtin.add_type(cx.alloc_toplevel_ident("u64"), Ty::mk_u64(cx));

    builtin.add_type(cx.alloc_toplevel_ident("f32"), Ty::mk_f32(cx));
    builtin.add_type(cx.alloc_toplevel_ident("f64"), Ty::mk_f64(cx));

    builtin.add_type(cx.alloc_toplevel_ident("string"), Ty::mk_string(cx));
    builtin.add_type(cx.alloc_toplevel_ident("bool"), Ty::mk_bool(cx));
    builtin.add_type(cx.alloc_toplevel_ident("duration"), Ty::mk_duration(cx));
    builtin.add_type(cx.alloc_toplevel_ident("time"), Ty::mk_time(cx));

    cx.alloc_module(builtin)
}

/// The main type checker that performs type inference and checking.
///
/// The type checker maintains state for generating fresh type variables
/// and manages module loading for resolving external types and functions.
pub struct TypeChecker<'cx> {
    /// Module loader for resolving external symbols
    module_loader: ModuleLoader<'cx>,
    typing_cx: &'cx TypingContext<'cx>,
    ir_cx: &'cx ir::Context<'cx>,
}

impl core::fmt::Debug for TypeChecker<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        let TypeChecker { module_loader, .. } = self;
        f.debug_struct("TypeChecker")
            .field("module_loader", &module_loader)
            .finish()
    }
}

/// Visitor for applying substitutions to all types in the IR
struct SubstitutionVisitor<'cx> {
    ty_last_seen: Option<Ty<'cx>>,
    subst: Substitution<'cx>,
    typing_cx: &'cx TypingContext<'cx>,
}

impl<'cx> SubstitutionVisitor<'cx> {
    fn new(subst: Substitution<'cx>, typing_cx: &'cx TypingContext<'cx>) -> Self {
        Self {
            subst,
            typing_cx,
            ty_last_seen: None,
        }
    }
}

opslang_ir_macro::v1_ir_visitor_impl!(for SubstitutionVisitor<'cx> {
    fn visit_expr_mut(&mut self, expr: &mut ir::Expr<'cx>) {
        use ir::IrMutVisitor;
        // visit ty first
        self.visit_ty_mut(&mut expr.ty);
        self.visit_mut(&mut expr.kind);
    }
    fn visit_ty_mut(&mut self, ty: &mut Ty<'cx>) {
        self.subst.apply_substitution(self.typing_cx, ty);
        let kind = ty.kind();
        match kind {
            TyKind::Infer(InferTy::IntVar(int_vid)) => {
                self.subst.resolve_int(*int_vid, IntTy::I32);
            }
            TyKind::Infer(InferTy::FloatVar(float_vid)) => {
                self.subst.resolve_float(*float_vid, FloatTy::F64);
            }
            _ => {}
        }
        self.subst.apply_substitution(self.typing_cx, ty);
        self.ty_last_seen = Some(*ty);
    }
    fn visit_ty(&mut self, _ty: &Ty<'cx>) {
        panic!("found `Ty` immutability. this prevents type variable resolution. review changes to IR structure and eliminate possession of immutable `Ty`pes.");
    }
    fn visit_numeric_mut(&mut self, numeric: &mut ir::Numeric<'cx>) {
        if let ir::NumericKind::Repr(unparsed) = numeric.kind {
            let literal = typeck_literal::parse_literal(unparsed, self.ty_last_seen.unwrap()).unwrap();
            numeric.kind = literal;
        }
    }
    fn visit_resolved_item_mut(&mut self, _resolved_item: &mut ir::ResolvedItem<'cx>) {
        // `ResolvedItem` is immutable, not calling super
    }
});

impl<'cx> TypeChecker<'cx> {
    /// Creates a new type checker with empty module loader.
    ///
    /// The type checker starts with no modules loaded.
    pub fn new(typing_cx: &'cx TypingContext<'cx>, ir_cx: &'cx ir::Context<'cx>) -> Self {
        Self {
            module_loader: ModuleLoader::new(),
            typing_cx,
            ir_cx,
        }
    }

    /// Creates a new type checker with the given module loader.
    ///
    /// This allows pre-loading modules before starting type checking operations.
    pub fn with_module_loader(
        module_loader: ModuleLoader<'cx>,
        typing_cx: &'cx TypingContext<'cx>,
        ir_cx: &'cx ir::Context<'cx>,
    ) -> Self {
        Self {
            module_loader,
            typing_cx,
            ir_cx,
        }
    }

    /// Adds a module to the type checker's module loader.
    ///
    /// This makes the module's exported items available for type resolution.
    pub fn add_module(&mut self, module: &'cx Module<'cx>) {
        self.module_loader.add_module(module);
    }

    /// Performs type checking on an AST program and converts it to IR.
    ///
    /// This is the main entry point for type checking and IR generation.
    /// It processes all top-level definitions, performs type inference,
    /// and converts the AST to a typed IR representation.
    pub fn typeck(
        &mut self,
        program: &ast::Program<'cx>,
    ) -> Result<ast::Program<'cx, IrTypeFamily>> {
        // Create global environment for top-level definitions
        let mut global_env = Environment::<'cx, '_>::new();

        // First pass: collect all function and constant signatures
        for definition in program.toplevel_items {
            if let Some(kind) = &definition.kind {
                match kind {
                    ast::DefinitionKind::Function(func_def) => {
                        self.register_function_signature(&mut global_env, func_def)?;
                    }
                    ast::DefinitionKind::Constant(const_def) => {
                        self.register_constant_signature(&mut global_env, const_def)?;
                    }
                }
            }
        }

        // Second pass: type check implementations and process comments
        let mut ir_definitions = Vec::new();
        let mut pending_comments: Vec<&'cx ast::Comment<'cx>> = Vec::new();

        for toplevel_item in program.toplevel_items {
            if toplevel_item.is_empty() {
                // Empty item (empty line) - discard any pending comments
                pending_comments.clear();
                continue;
            }

            // Collect comment from this item if present
            if let Some(comment) = &toplevel_item.comment {
                pending_comments.push(comment);
            }

            // If this item has a definition, create an IR Definition
            if let Some(kind) = &toplevel_item.kind {
                let leading_comment = if pending_comments.is_empty() {
                    None
                } else {
                    Some(self.merge_comments(&pending_comments)?)
                };

                let ir_kind = match kind {
                    ast::DefinitionKind::Function(func_def) => {
                        let ir_func = self.typeck_function(&global_env, func_def)?;
                        ast::DefinitionKind::Function(ir_func)
                    }
                    ast::DefinitionKind::Constant(const_def) => {
                        let ir_const = self.typeck_constant(&global_env, const_def)?;
                        ast::DefinitionKind::Constant(ir_const)
                    }
                };

                let ir_definition = ir::Definition {
                    comment_before: leading_comment,
                    comment_trailing: None, // TODO: Handle trailing comments
                    kind: ir_kind,
                };

                ir_definitions.push(ir_definition);
                pending_comments.clear();
            }
        }

        Ok(ast::Program {
            toplevel_items: self.ir_cx.alloc_definition_slice(ir_definitions),
        })
    }

    fn register_function_signature(
        &mut self,
        env: &mut Environment<'cx, '_>,
        func_def: &ast::FunctionDef<'cx>,
    ) -> Result<()> {
        let func_name = func_def.name;

        // Check if function with same name already exists
        if env.lookup_name(func_name).is_some() {
            return Err(anyhow!("Function '{func_name}' is already defined"));
        }

        let mut param_types = Vec::new();

        for param in func_def.parameters {
            let param_type = self.resolve_type_from_path(param.ty)?;
            param_types.push(param_type);
        }

        // Handle return type from function definition
        let return_type = match &func_def.return_type.0 {
            Some((_, return_path)) => self.resolve_type_from_path(*return_path)?,
            None => Ty::mk_unit(self.typing_cx),
        };

        let func_type = Ty::mk_function(self.typing_cx, param_types, return_type);

        let func_name = func_name.raw;

        let func_identifier_id = self.typing_cx.alloc_identifier(func_name);
        env.bind(func_name, func_identifier_id, func_type);

        Ok(())
    }

    fn register_constant_signature(
        &mut self,
        env: &mut Environment<'cx, '_>,
        const_def: &ast::ConstantDef<'cx>,
    ) -> Result<()> {
        let const_name = const_def.name.raw;
        let declared_type = self.resolve_type_from_path(const_def.ty)?;

        let const_identifier_id = self.typing_cx.alloc_identifier(const_name);
        env.bind(const_name, const_identifier_id, declared_type);

        Ok(())
    }

    fn resolve_type_from_path(&self, path: ast::Path<'cx>) -> Result<Ty<'cx>> {
        match self.module_loader.resolve_path(path) {
            Some(item) => match item {
                opslang_ty::version::v1::ModuleItem::Type { ty, .. } => Ok(*ty),
                _ => Err(anyhow!("path `{path}` does not refer to a type")),
            },
            None => Err(anyhow!("unknown type: {path}")),
        }
    }
}

/// Result of [`TypeChecker::typeck_row`] - either a comment to be merged or a regular scope item
enum RowProcessResult<'cx> {
    Comment(&'cx ast::Comment<'cx>),
    ScopeItem(ast::ScopeItem<'cx, IrTypeFamily>),
}

impl<'cx> TypeChecker<'cx> {
    fn typeck_row(
        &mut self,
        env: &mut Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        row: &ast::Row<'cx>,
    ) -> Result<RowProcessResult<'cx>> {
        // Check if this row is only a comment (no content, no breaks)
        if row.breaks.is_none()
            && row.statement.is_none()
            && let Some(comment) = &row.comment
        {
            return Ok(RowProcessResult::Comment(comment));
        }

        // Process as a regular row
        let ir_content = if let Some(content) = &row.statement {
            let stmt = self.typeck_statement(env, subst, content)?;
            Some(stmt)
        } else {
            None
        };

        let ir_comment = row.comment.as_ref().map(|comment| ir::Comment {
            content: comment.content,
            span: comment.span,
            source_comments: self.ir_cx.alloc_ast_comment_slice(&[comment]),
        });

        let ir_row = ast::Row {
            breaks: row.breaks.map(|b| b.into_token()),
            statement: ir_content,
            comment: ir_comment,
        };

        Ok(RowProcessResult::ScopeItem(ast::ScopeItem::Row(ir_row)))
    }

    fn typeck_block(
        &mut self,
        env: &Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        block: &ast::Block<'cx>,
    ) -> Result<ast::Block<'cx, IrTypeFamily>> {
        let mut local_env = env.extend_inherit();
        let mut ir_items: Vec<ast::ScopeItem<'cx, IrTypeFamily>> = Vec::new();
        let mut pending_comments: Vec<&'cx ast::Comment<'cx>> = Vec::new();

        for item in block.scope.items {
            match item {
                ast::ScopeItem::Row(row) => {
                    match self.typeck_row(&mut local_env, subst, row)? {
                        RowProcessResult::Comment(comment) => {
                            pending_comments.push(comment);
                        }
                        RowProcessResult::ScopeItem(scope_item) => {
                            // Flush any pending comments before adding the regular item
                            self.flush_comments_to_items(&mut pending_comments, &mut ir_items)?;
                            ir_items.push(scope_item);
                        }
                    }
                }
                ast::ScopeItem::Block(nested_block) => {
                    // Flush any pending comments before adding the block
                    self.flush_comments_to_items(&mut pending_comments, &mut ir_items)?;
                    let ir_block = self.typeck_block(&local_env, subst, nested_block)?;
                    ir_items.push(ast::ScopeItem::Block(ir_block));
                }
            }
        }

        // Flush any remaining comments at the end
        self.flush_comments_to_items(&mut pending_comments, &mut ir_items)?;

        let ir_scope = ir::Scope { items: ir_items };

        let ir_block = ast::Block {
            left_brace: block.left_brace.into_token(),
            scope: ir_scope,
            right_brace: block.right_brace.into_token(),
        };

        Ok(ir_block)
    }

    fn flush_comments_to_items(
        &self,
        comments: &mut Vec<&'cx ast::Comment<'cx>>,
        ir_items: &mut Vec<ast::ScopeItem<'cx, IrTypeFamily>>,
    ) -> Result<()> {
        if !comments.is_empty() {
            let merged_comment = self.merge_comments(comments)?;
            let comment_row = ast::Row {
                breaks: None,
                statement: None,
                comment: Some(merged_comment),
            };
            ir_items.push(ast::ScopeItem::Row(comment_row));
            comments.clear();
        }
        Ok(())
    }

    fn merge_comments(&self, comments: &[&'cx ast::Comment<'cx>]) -> Result<ir::Comment<'cx>> {
        lower::merge_comments(self.ir_cx, comments)
    }

    fn typeck_statement(
        &mut self,
        env: &mut Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        stmt: &ast::Statement<'cx>,
    ) -> Result<ast::Statement<'cx, IrTypeFamily>> {
        match stmt {
            ast::Statement::Let(let_stmt) => {
                let ir_rhs = self.typeck_expr(env, subst, &let_stmt.rhs)?;

                // Bind the variable to the environment with the inferred type
                let var_name = let_stmt.variable.raw;
                let var_identifier_id = self.typing_cx.alloc_identifier(var_name);
                env.bind(var_name, var_identifier_id, ir_rhs.ty);

                Ok(ast::Statement::Let(ast::Let {
                    let_token: let_stmt.let_token.into_token(),
                    variable: self.resolve_ident(let_stmt.variable)?,
                    eq: let_stmt.eq.into_token(),
                    rhs: ir_rhs,
                    semi: let_stmt.semi.into_token(),
                }))
            }
            ast::Statement::Expr(expr_stmt) => {
                let ir_expr = self.typeck_expr(env, subst, &expr_stmt.expr)?;

                Ok(ast::Statement::Expr(ast::ExprStatement {
                    expr: ir_expr,
                    semi: expr_stmt.semi.into_token(),
                }))
            }
            ast::Statement::Return(ret_stmt) => Ok(ast::Statement::Return(ret_stmt.into_token())),
        }
    }

    fn resolve_ident(&self, ident: ast::Ident<'cx>) -> Result<Ident<'cx>> {
        // For now, create a new identifier with a unique definition ID
        // TODO: This should probably lookup from environment instead
        Ok(self.typing_cx.alloc_identifier(ident.raw))
    }

    fn resolve_path(&self, path: &'cx ast::Path<'cx>) -> Result<ResolvePathResult<'cx>> {
        match self.module_loader.resolve_path(*path) {
            Some(item) => {
                let resolved_path = ir::ResolvedPath {
                    item: opslang_ir::version::ResolvedItem::ModuleItem(item),
                    original_path: path,
                };
                Ok(ResolvePathResult {
                    resolved_path,
                    item,
                })
            }
            None => Err(anyhow!("cannot resolve path: {path}")),
        }
    }
}

/// Result of path resolution containing both the IR representation and the original item.
#[derive(Debug, Clone, Copy)]
struct ResolvePathResult<'cx> {
    /// The resolved path for IR generation
    resolved_path: ir::ResolvedPath<'cx>,
    /// The original module item with type information
    item: &'cx ModuleItem<'cx>,
}

#[cfg(test)]
mod tests {
    use opslang_ty::version::{IntTy, ModuleItem};

    use super::*;

    fn parse_ident<'cx>(cx: &'cx ast::context::Context<'cx>, str: &str) -> ast::Path<'cx> {
        ast::Path::single(
            cx,
            str,
            ast::Span {
                start: ast::BytePos(0),
                end: ast::BytePos(0),
            },
        )
    }

    #[test]
    fn test_typing_context() {
        let cx = TypingContext::new();

        let int_type = Ty::mk_i32(&cx);
        let array_type = Ty::mk_array(&cx, int_type);

        assert_eq!(cx.display_type(int_type), "i32");
        assert_eq!(cx.display_type(array_type), "[i32]");

        let id = cx.alloc_identifier("test");
        assert_eq!(id.name, "test");
    }

    #[test]
    fn test_builtin_module() {
        let cx = TypingContext::new();
        let ast_cx = ast::context::Context::new();
        let builtin = create_builtin_module(&cx);

        assert_eq!(builtin.name(), "builtin");
        assert!(builtin.lookup_item(parse_ident(&ast_cx, "i32")).is_some());
        assert!(builtin.lookup_item(parse_ident(&ast_cx, "f64")).is_some());
        assert!(
            builtin
                .lookup_item(parse_ident(&ast_cx, "unknown"))
                .is_none()
        );

        if let Some(ModuleItem::Type { id, ty }) = builtin.lookup_item(parse_ident(&ast_cx, "i32"))
        {
            assert_eq!(id.name, "i32");
            assert!(matches!(ty.kind(), TyKind::Int(IntTy::I32)));
        }
    }

    #[test]
    fn test_module_loader() {
        let cx = TypingContext::new();
        let ast_cx = ast::context::Context::new();
        let builtin = create_builtin_module(&cx);

        let mut loader = ModuleLoader::new();
        loader.add_module(builtin);

        assert!(loader.lookup_module("builtin").is_some());
        assert!(loader.lookup_module("unknown").is_none());

        assert!(loader.resolve_path(parse_ident(&ast_cx, "i32")).is_some());
        assert!(
            loader
                .resolve_path(parse_ident(&ast_cx, "unknown"))
                .is_none()
        );
    }

    #[test]
    fn test_type_checker_with_modules() {
        let cx = TypingContext::new();
        let ast_cx = ast::context::Context::new();
        let ir_cx = ir::Context::new();
        let builtin = create_builtin_module(&cx);

        let mut loader = ModuleLoader::new();
        loader.add_module(builtin);

        let checker = TypeChecker::with_module_loader(loader, &cx, &ir_cx);

        let i32_type = checker
            .resolve_type_from_path(parse_ident(&ast_cx, "i32"))
            .unwrap();
        assert!(matches!(i32_type.kind(), TyKind::Int(IntTy::I32)));

        let unknown_result = checker.resolve_type_from_path(parse_ident(&ast_cx, "unknown"));
        assert!(unknown_result.is_err());
    }

    #[test]
    fn test_substitution() {
        let cx = TypingContext::new();
        let var = TyVid::fresh();
        let int_type = Ty::mk_i32(&cx);

        let mut subst = Substitution::new();
        subst.insert(var, int_type);

        let var_type = Ty::mk_variable(&cx, var);
        let result = subst.apply_substitution_pure(&cx, var_type);

        assert!(matches!(result.kind(), TyKind::Int(IntTy::I32)));
    }

    #[test]
    fn test_unify_basic() {
        let cx = TypingContext::new();
        let ir_cx = ir::Context::new();

        let int_type1 = Ty::mk_i32(&cx);
        let int_type2 = Ty::mk_i32(&cx);
        let float_type = Ty::mk_f64(&cx);

        let chk = TypeChecker::new(&cx, &ir_cx);

        let result = chk.unify_pure(int_type1, int_type2);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());

        let result = chk.unify_pure(int_type1, float_type);
        assert!(result.is_err());
    }

    #[test]
    fn test_variable_resolution_priority() {
        let cx = TypingContext::new();
        let ir_cx = ir::Context::new();
        let builtin = create_builtin_module(&cx);

        let mut loader = ModuleLoader::new();
        loader.add_module(builtin);
        let mut _checker = TypeChecker::with_module_loader(loader, &cx, &ir_cx);

        let mut env = Environment::<'_, '_>::new();

        // ローカル変数 "i32" を定義（組み込み型をシャドーイング）
        let local_i32_type = Ty::mk_string(&cx);
        let local_id = cx.alloc_identifier("i32");
        env.bind("i32", local_id, local_i32_type);

        // パスを作成して型を解決
        // let path = ast::Path {
        //     raw: "i32",
        //     segments: &[],
        // };

        // let (resolved_type, _) = checker.typeck_variable(&cx, &env, &path).unwrap();

        // // ローカル変数が優先されるべき（String型になっている）
        // assert!(matches!(resolved_type.kind(), TyKind::String));
    }
}
