use anyhow::anyhow;
use chrono::Utc;
use opslang_ast::v1::token::IntoToken;
use opslang_ast::v1::{self as ast, ExprKind, Statement};
use opslang_ir::version::v1::{self as ir, NumericKind, ResolvedPath};
use opslang_ir::version::{IrTypeFamily, Typed};
use opslang_ty::version::v1::{
    Ident, Identifier, Module, ModuleItem, ModuleLoader, Ty, TyKind, TypeVariable, TypingContext,
};
use std::collections::HashMap;

type Result<T, E = anyhow::Error> = std::result::Result<T, E>;

mod hm;
use hm::Substitution;

/// Creates the builtin module containing all primitive types.
///
/// The builtin module provides access to fundamental types like integers, strings,
/// and other primitives that are available in all contexts without explicit imports.
pub fn create_builtin_module<'cx>(cx: &'cx TypingContext<'cx>) -> &'cx Module<'cx> {
    let mut builtin = Module::new(cx.alloc_toplevel_ident("builtin"));

    // Add all builtin primitive types
    builtin.add_item(ModuleItem::Type {
        id: cx.alloc_toplevel_ident("i32"),
        ty: Ty::mk_int(cx),
    });
    builtin.add_item(ModuleItem::Type {
        id: cx.alloc_toplevel_ident("f64"),
        ty: Ty::mk_float(cx),
    });
    builtin.add_item(ModuleItem::Type {
        id: cx.alloc_toplevel_ident("string"),
        ty: Ty::mk_string(cx),
    });
    builtin.add_item(ModuleItem::Type {
        id: cx.alloc_toplevel_ident("bool"),
        ty: Ty::mk_bool(cx),
    });
    builtin.add_item(ModuleItem::Type {
        id: cx.alloc_toplevel_ident("duration"),
        ty: Ty::mk_duration(cx),
    });
    builtin.add_item(ModuleItem::Type {
        id: cx.alloc_toplevel_ident("time"),
        ty: Ty::mk_time(cx),
    });

    cx.alloc_module(builtin)
}

/// Represents a lexical environment for name and type bindings.
///
/// Environments form a chain through parent references, enabling proper
/// lexical scoping where inner scopes can shadow outer scope bindings.
#[derive(Debug)]
pub struct Environment<'cx, 'env> {
    /// Maps variable names to their unique identifiers
    name_bindings: HashMap<&'cx str, Ident<'cx>>,
    /// Maps identifiers to their types
    type_bindings: HashMap<Ident<'cx>, Ty<'cx>>,
    /// Reference to parent environment for scope chaining
    parent: Option<&'env Self>,
    /// Nesting depth of this scope
    scope_depth: usize,
}

impl<'cx, 'env> Environment<'cx, 'env> {
    /// Creates a new top-level environment with no parent.
    ///
    /// This represents the global scope and has depth 0.
    pub fn new() -> Self {
        Self {
            name_bindings: HashMap::new(),
            type_bindings: HashMap::new(),
            parent: None,
            scope_depth: 0,
        }
    }

    /// Creates a new environment that extends a parent environment.
    ///
    /// The new environment has one greater depth than its parent and can access
    /// bindings from the parent chain while allowing local shadowing.
    pub fn extend_inherit(&'env self) -> Self {
        let scope_depth = self.scope_depth + 1;
        Self {
            name_bindings: HashMap::new(),
            type_bindings: HashMap::new(),
            parent: Some(self),
            scope_depth,
        }
    }

    /// Binds a name to an identifier and associates the identifier with a type.
    ///
    /// This is a convenience method that performs both name and type binding in one operation.
    pub fn bind(&mut self, name: &'cx str, id: Ident<'cx>, ty: Ty<'cx>) {
        self.name_bindings.insert(name, id);
        self.type_bindings.insert(id, ty);
    }

    /// Binds a name to an identifier without associating a type.
    ///
    /// This is used when the type will be bound separately or is not yet known.
    pub fn bind_name(&mut self, name: &'cx str, id: Ident<'cx>) {
        self.name_bindings.insert(name, id);
    }

    /// Associates an identifier with a type.
    ///
    /// This is used when the identifier is already known but its type needs to be recorded.
    pub fn bind_type(&mut self, id: Ident<'cx>, ty: Ty<'cx>) {
        self.type_bindings.insert(id, ty);
    }

    /// Looks up a name to find its associated identifier.
    ///
    /// Searches the current environment first, then walks up the parent chain.
    /// Returns None if the name is not bound in any accessible scope.
    pub fn lookup_name(&self, name: &str) -> Option<Ident<'cx>> {
        self.name_bindings
            .get(name)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.lookup_name(name)))
    }

    /// Looks up the type associated with an identifier.
    ///
    /// Searches the current environment first, then walks up the parent chain.
    /// Returns None if the identifier is not associated with any type in accessible scopes.
    pub fn lookup_type(&self, id: Ident<'cx>) -> Option<Ty<'cx>> {
        self.type_bindings
            .get(&id)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.lookup_type(id)))
    }

    /// Looks up a variable by name and returns its type.
    ///
    /// This combines name lookup and type lookup into a single operation,
    /// which is the most common operation during type checking.
    pub fn lookup_variable(&self, name: &str) -> Option<Ty<'cx>> {
        if let Some(id) = self.lookup_name(name) {
            self.lookup_type(id)
        } else {
            None
        }
    }

    /// Returns the nesting depth of this environment.
    ///
    /// The global environment has depth 0, with each nested scope incrementing the depth.
    pub fn scope_depth(&self) -> usize {
        self.scope_depth
    }
}

impl<'cx, 'env> Default for Environment<'cx, 'env> {
    fn default() -> Self {
        Self::new()
    }
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

    /// Generates a fresh type variable with a unique identifier.
    ///
    /// Fresh type variables are used during type inference to represent unknown types
    /// that will be unified with concrete types during the checking process.
    pub fn fresh_type_var(&mut self) -> TypeVariable {
        TypeVariable::fresh()
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
        for definition in program.definitions {
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

        // Second pass: type check implementations
        let mut ir_definitions = Vec::new();
        for definition in program.definitions {
            let kind = if let Some(kind) = &definition.kind {
                let kind = match kind {
                    ast::DefinitionKind::Function(func_def) => {
                        let ir_func = self.typeck_function(&global_env, func_def)?;
                        ast::DefinitionKind::Function(ir_func)
                    }
                    ast::DefinitionKind::Constant(const_def) => {
                        let ir_const = self.typeck_constant(&global_env, const_def)?;
                        ast::DefinitionKind::Constant(ir_const)
                    }
                };
                Some(kind)
            } else {
                None
            };
            ir_definitions.push(ast::Definition {
                kind,
                // FIXME
                comment: None,
            });
        }

        Ok(ast::Program {
            definitions: self.ir_cx.alloc_definition_slice(ir_definitions),
        })
    }

    fn register_function_signature(
        &mut self,
        env: &mut Environment<'cx, '_>,
        func_def: &ast::FunctionDef<'cx>,
    ) -> Result<()> {
        let func_name = func_def.name.raw;

        // Check if function with same name already exists
        if env.lookup_name(func_name).is_some() {
            return Err(anyhow!("Function '{func_name}' is already defined"));
        }

        let mut param_types = Vec::new();

        for param in func_def.parameters {
            let param_type = self.resolve_type_from_path(param.ty.raw)?;
            param_types.push(param_type);
        }

        // Handle return type from function definition
        let return_type = match &func_def.return_type.0 {
            Some((_, return_path)) => self.resolve_type_from_path(return_path.raw)?,
            None => Ty::mk_unit(self.typing_cx),
        };

        let func_type = Ty::mk_function(self.typing_cx, param_types, return_type);

        let func_identifier = Identifier {
            name: func_name,
            scope_depth: env.scope_depth(),
        };
        let func_identifier_id = self.typing_cx.alloc_ident(func_identifier);
        env.bind(func_name, func_identifier_id, func_type);

        Ok(())
    }

    fn register_constant_signature(
        &mut self,
        env: &mut Environment<'cx, '_>,
        const_def: &ast::ConstantDef<'cx>,
    ) -> Result<()> {
        let const_name = const_def.name.raw;
        let declared_type = self.resolve_type_from_path(const_def.ty.raw)?;

        let const_identifier = Identifier {
            name: const_name,
            scope_depth: env.scope_depth(),
        };
        let const_identifier_id = self.typing_cx.alloc_ident(const_identifier);
        env.bind(const_name, const_identifier_id, declared_type);

        Ok(())
    }

    fn typeck_function(
        &mut self,
        global_env: &Environment<'cx, '_>,
        func_def: &'cx ast::FunctionDef<'cx>,
    ) -> Result<ast::FunctionDef<'cx, IrTypeFamily>> {
        let func_name = func_def.name.raw;

        // Retrieve the already resolved function type from the environment
        let func_type = global_env
            .lookup_variable(func_name)
            .ok_or_else(|| anyhow!("Function '{func_name}' not found in environment"))?;

        let (param_types, return_type) = match func_type.kind() {
            TyKind::Function { arg, ret } => (arg.clone(), *ret),
            _ => return Err(anyhow!("Expected function type for '{func_name}'")),
        };

        let mut func_env = global_env.extend_inherit();

        // Convert parameters to IR using the already resolved types
        let mut ir_parameters = Vec::new();
        for (param, param_type) in func_def.parameters.iter().zip(param_types.iter()) {
            let param_name = param.name.raw;

            let param_identifier = Identifier {
                name: param_name,
                scope_depth: func_env.scope_depth(),
            };
            let param_identifier_id = self.typing_cx.alloc_ident(param_identifier);
            func_env.bind(param_name, param_identifier_id, *param_type);

            let ir_param = ast::Parameter {
                name: param_identifier_id,
                colon: param.colon.into_token(),
                ty: self.resolve_path(&param.ty)?.item.ty(),
            };
            ir_parameters.push(ir_param);
        }

        // Type check function body
        let ir_body = self.typeck_block(&func_env, &mut Substitution::new(), func_def.body)?;

        Ok(ast::FunctionDef {
            prc_token: func_def.prc_token.into_token(),
            name: self.resolve_ident(func_def.name)?,
            left_paren: func_def.left_paren.into_token(),
            parameters: self.ir_cx.alloc_parameter_slice(ir_parameters),
            right_paren: func_def.right_paren.into_token(),
            return_type,
            body: ir_body,
        })
    }

    fn typeck_constant(
        &mut self,
        env: &Environment<'cx, '_>,
        const_def: &'cx ast::ConstantDef<'cx>,
    ) -> Result<ast::ConstantDef<'cx, IrTypeFamily>> {
        let declared_type = self.resolve_type_from_path(const_def.ty.raw)?;
        let mut subst = Substitution::new();
        let subst = &mut subst;
        let mut inferred_expr = self.typeck_expr(env, subst, &const_def.value)?;

        self.unify(subst, declared_type, inferred_expr.ty)?;
        subst.apply_substitution(self.typing_cx, &mut inferred_expr.ty);

        let ir_ty = self.resolve_path(&const_def.ty)?;

        Ok(ast::ConstantDef {
            const_token: const_def.const_token.into_token(),
            name: self.resolve_ident(const_def.name)?,
            colon: const_def.colon.into_token(),
            ty: ir_ty,
            eq: const_def.eq.into_token(),
            value: inferred_expr,
            semi: const_def.semi.into_token(),
        })
    }

    fn resolve_type_from_path(&self, path: &str) -> Result<Ty<'cx>> {
        match self.module_loader.resolve_path(path) {
            Some(ModuleItem::Type { ty, .. }) => Ok(*ty),
            Some(_) => Err(anyhow!("Path '{path}' does not refer to a type")),
            None => Err(anyhow!("Unknown type: {path}")),
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
        env: &Environment<'cx, '_>,
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

        let ir_comment = if let Some(comment) = &row.comment {
            let converted_comment = self.convert_comment(comment)?;
            Some(converted_comment)
        } else {
            None
        };

        let ir_row = ast::Row {
            breaks: row.breaks.map(|b| b.into_token()),
            statement: ir_content,
            comment: ir_comment,
        };

        Ok(RowProcessResult::ScopeItem(ast::ScopeItem::Row(
            self.ir_cx.alloc_row(ir_row),
        )))
    }

    fn convert_comment(&self, comment: &'cx ast::Comment<'cx>) -> Result<ir::Comment<'cx>> {
        // For single comment conversion (used when comment is part of a regular row)
        Ok(ir::Comment {
            content: comment.content,
            span: comment.span,
            source_comments: &[],
        })
    }

    fn typeck_block(
        &mut self,
        env: &Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        block: &ast::Block<'cx>,
    ) -> Result<&'cx ast::Block<'cx, IrTypeFamily>> {
        let local_env = env.extend_inherit();
        let mut ir_items = Vec::new();
        let mut pending_comments: Vec<&'cx ast::Comment<'cx>> = Vec::new();

        for item in block.scope.items {
            match item {
                ast::ScopeItem::Row(row) => {
                    match self.typeck_row(&local_env, subst, row)? {
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

        let ir_scope = ast::Scope {
            items: self.ir_cx.alloc_scope_item_slice(ir_items),
        };

        let ir_block = ast::Block {
            left_brace: block.left_brace.into_token(),
            scope: ir_scope,
            right_brace: block.right_brace.into_token(),
        };

        Ok(self.ir_cx.alloc_block(ir_block))
    }

    fn flush_comments_to_items(
        &self,
        comments: &mut Vec<&'cx ast::Comment<'cx>>,
        ir_items: &mut Vec<ast::ScopeItem<'cx, IrTypeFamily>>,
    ) -> Result<()> {
        if !comments.is_empty() {
            let merged_comment = self.merge_comments(self.ir_cx, comments)?;
            let comment_row = ast::Row {
                breaks: None,
                statement: None,
                comment: Some(merged_comment),
            };
            ir_items.push(ast::ScopeItem::Row(self.ir_cx.alloc_row(comment_row)));
            comments.clear();
        }
        Ok(())
    }

    fn merge_comments(
        &self,
        ir_cx: &'cx ir::Context<'cx>,
        comments: &[&'cx ast::Comment<'cx>],
    ) -> Result<ir::Comment<'cx>> {
        if comments.is_empty() {
            return Err(anyhow!("Cannot merge empty comment list"));
        }

        if comments.len() == 1 {
            // Single comment - simple case
            return Ok(ir::Comment {
                content: comments[0].content,
                span: comments[0].span,
                source_comments: &[],
            });
        }

        // Multiple comments - merge content and spans
        let mut merged_content = String::new();
        let start = comments[0].span.start;
        let mut end = comments[0].span.end;

        for (i, comment) in comments.iter().enumerate() {
            if i > 0 {
                merged_content.push('\n');
            }
            merged_content.push_str(comment.content);

            // Track the overall span from first to last
            if i == comments.len() - 1 {
                end = comment.span.end;
            }
        }

        let merged_span = ast::Span { start, end };

        // Allocate the merged content in the context
        let content_ref = ir_cx.alloc_str(&merged_content);

        Ok(ir::Comment {
            content: content_ref,
            span: merged_span,
            source_comments: &[],
        })
    }

    fn typeck_statement(
        &mut self,
        env: &Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        stmt: &Statement<'cx>,
    ) -> Result<Statement<'cx, IrTypeFamily>> {
        match stmt {
            Statement::Let(let_stmt) => {
                let ir_rhs = self.typeck_expr(env, subst, &let_stmt.rhs)?;

                Ok(Statement::Let(ast::Let {
                    let_token: let_stmt.let_token.into_token(),
                    variable: self.resolve_ident(let_stmt.variable)?,
                    eq: let_stmt.eq.into_token(),
                    rhs: ir_rhs,
                    semi: let_stmt.semi.into_token(),
                }))
            }
            Statement::Expr(expr_stmt) => {
                let ir_expr = self.typeck_expr(env, subst, &expr_stmt.expr)?;

                Ok(Statement::Expr(ast::ExprStatement {
                    expr: ir_expr,
                    semi: expr_stmt.semi.into_token(),
                }))
            }
            Statement::Return(ret_stmt) => Ok(Statement::Return(ret_stmt.into_token())),
        }
    }

    /// Performs type checking on an expression and converts it to IR.
    ///
    /// This function infers the type of an expression and converts it to its IR representation.
    /// It updates the provided substitution with any new type constraints discovered during checking.
    fn typeck_expr(
        &mut self,
        env: &Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        mut expr: &'cx ast::ExprKind<'cx>,
    ) -> Result<ir::Expr<'cx>> {
        // Peel parentheses
        while let ExprKind::Parened(parened) = expr {
            expr = &parened.expr;
        }
        match expr {
            ExprKind::Parened(_parened) => unreachable!("handled above"),
            ExprKind::Literal(literal) => self.typeck_literal(env, subst, literal),
            ExprKind::Variable(path) => {
                let var_name = path.raw;

                let inferred_type = if let Some(type_ref) = env.lookup_variable(var_name) {
                    type_ref
                } else {
                    match self.module_loader.resolve_path(var_name) {
                        Some(item) => item.ty(),
                        None => return Err(anyhow!("Unbound variable: {var_name}")),
                    }
                };

                let resolved_path = self.resolve_path(path)?;
                let ir_expr = ir::Expr::new(
                    ast::ExprMut::variable(self.ir_cx, resolved_path),
                    inferred_type,
                );
                Ok(ir_expr)
            }
            ExprKind::Binary(binary) => {
                let mut lhs_ir = self.typeck_expr(env, subst, &binary.lhs)?;
                let mut rhs_ir = self.typeck_expr(env, subst, &binary.rhs)?;
                subst.apply_substitution(self.typing_cx, &mut lhs_ir.ty);
                subst.apply_substitution(self.typing_cx, &mut rhs_ir.ty);

                match binary.op {
                    ast::BinOp::Add(_)
                    | ast::BinOp::Sub(_)
                    | ast::BinOp::Mul(_)
                    | ast::BinOp::Div(_)
                    | ast::BinOp::Mod(_) => {
                        self.unify(subst, lhs_ir.ty, lhs_ir.ty)?;
                        subst.apply_substitution(self.typing_cx, &mut lhs_ir.ty);
                        match lhs_ir.ty.kind() {
                            TyKind::Int | TyKind::Float => {
                                // Apply final substitution to operands
                                subst.apply_substitution(self.typing_cx, &mut lhs_ir.ty);
                                subst.apply_substitution(self.typing_cx, &mut rhs_ir.ty);

                                // Create IR binary expression
                                let ty = lhs_ir.ty;
                                let ir_expr = ir::Expr::new(
                                    ast::ExprMut::binary(
                                        self.ir_cx,
                                        lhs_ir,
                                        binary.op.into_token(),
                                        rhs_ir,
                                    ),
                                    ty,
                                );
                                Ok(ir_expr)
                            }
                            _ => Err(anyhow!(
                                "Arithmetic operation requires numeric type, got {}",
                                lhs_ir.ty.display(self.typing_cx)
                            )),
                        }
                    }
                    ast::BinOp::And(_) | ast::BinOp::Or(_) => {
                        let bool_type = Ty::mk_bool(self.typing_cx);
                        self.unify(subst, lhs_ir.ty, bool_type)?;
                        self.unify(subst, lhs_ir.ty, bool_type)?;

                        // Apply final substitution to operands
                        subst.apply_substitution(self.typing_cx, &mut lhs_ir.ty);
                        subst.apply_substitution(self.typing_cx, &mut rhs_ir.ty);

                        // Create IR binary expression
                        let ir_expr = ir::Expr::new(
                            ast::ExprMut::binary(
                                self.ir_cx,
                                lhs_ir,
                                binary.op.into_token(),
                                rhs_ir,
                            ),
                            bool_type,
                        );
                        Ok(ir_expr)
                    }
                    ast::BinOp::In(_) => {
                        // For 'in' operator, lhs is an element and rhs should be a collection
                        // The result type is always bool
                        let bool_type = Ty::mk_bool(self.typing_cx);

                        // Check that rhs is an array type
                        match lhs_ir.ty.kind() {
                            TyKind::Array { inner } => {
                                // Unify lhs type with array element type
                                self.unify(subst, lhs_ir.ty, *inner)?;
                                // Apply final substitution to operands
                                subst.apply_substitution(self.typing_cx, &mut lhs_ir.ty);
                                subst.apply_substitution(self.typing_cx, &mut rhs_ir.ty);

                                // Create IR binary expression
                                let ir_expr = ir::Expr::new(
                                    ast::ExprMut::binary(
                                        self.ir_cx,
                                        lhs_ir,
                                        binary.op.into_token(),
                                        rhs_ir,
                                    ),
                                    bool_type,
                                );
                                Ok(ir_expr)
                            }
                            _ => Err(anyhow!(
                                "'in' operator requires array on right side, got {}",
                                lhs_ir.ty.display(self.typing_cx)
                            )),
                        }
                    }
                }
            }
            ExprKind::Unary(unary) => {
                let mut expr_ir = self.typeck_expr(env, subst, &unary.expr)?;

                match unary.op {
                    ast::UnOp::Neg(_) => {
                        subst.apply_substitution(self.typing_cx, &mut expr_ir.ty);
                        match expr_ir.ty.kind() {
                            TyKind::Int | TyKind::Float => {
                                // Ok, do nothing
                            }
                            _ => {
                                // resolve to int
                                self.unify(subst, expr_ir.ty, Ty::mk_int(self.typing_cx))?
                            }
                        }
                        // Create IR operand with unified type
                        subst.apply_substitution(self.typing_cx, &mut expr_ir.ty);

                        // Create IR unary expression
                        let ty = expr_ir.ty;
                        let ir_result = ir::Expr::new(
                            ast::ExprMut::unary(self.ir_cx, unary.op.into_token(), expr_ir),
                            ty,
                        );
                        Ok(ir_result)
                    }
                    ast::UnOp::IdRef(_) => {
                        // IdRef (&expr) - creates a reference to the expression
                        // For now, we'll implement this as a simple unary operation
                        // The type system may need extension for proper reference types
                        subst.apply_substitution(self.typing_cx, &mut expr_ir.ty);

                        // Create IR unary expression - result type is the same for now
                        let ty = expr_ir.ty;
                        let ir_result = ir::Expr::new(
                            ast::ExprMut::unary(self.ir_cx, unary.op.into_token(), expr_ir),
                            ty,
                        );
                        Ok(ir_result)
                    }
                    ast::UnOp::Deref(_) => {
                        // Deref ($expr) - dereferences a reference
                        // For now, we'll implement this as a simple unary operation
                        // The type system may need extension for proper reference types
                        subst.apply_substitution(self.typing_cx, &mut expr_ir.ty);
                        // Create IR unary expression - result type is the same for now
                        let ty = expr_ir.ty;
                        let ir_result = ir::Expr::new(
                            ast::ExprMut::unary(self.ir_cx, unary.op.into_token(), expr_ir),
                            ty,
                        );
                        Ok(ir_result)
                    }
                }
            }
            ExprKind::Apply(apply) => self.typeck_apply(env, subst, apply, &[]),
            ExprKind::If(if_expr) => {
                let mut cond_ir = self.typeck_expr(env, subst, &if_expr.cond)?;
                let bool_type = Ty::mk_bool(self.typing_cx);
                self.unify(subst, cond_ir.ty, bool_type)?;
                let ir_then_block = self.typeck_block(env, subst, if_expr.then_clause)?;

                if let Some(else_clause) = &if_expr.else_opt {
                    let ir_else_block = self.typeck_block(env, subst, else_clause.else_clause)?;
                    let unified_then = subst.apply_substitution_pure(
                        self.typing_cx,
                        ir_then_block
                            .ty(self.typing_cx)
                            .unwrap_or(Ty::mk_unit(self.typing_cx)),
                    );
                    let unified_else = subst.apply_substitution_pure(
                        self.typing_cx,
                        ir_else_block
                            .ty(self.typing_cx)
                            .unwrap_or(Ty::mk_unit(self.typing_cx)),
                    );
                    self.unify(subst, unified_then, unified_else)?;
                    let result_type = subst.apply_substitution_pure(self.typing_cx, unified_then);

                    // Convert condition to IR
                    subst.apply_substitution(self.typing_cx, &mut cond_ir.ty);

                    // Create IR if-else expression
                    let ir_expr = ir::Expr::new(
                        ast::ExprMut::if_then_else(
                            self.ir_cx,
                            if_expr.if_kw.into_token(),
                            cond_ir,
                            ir_then_block,
                            else_clause.else_kw.into_token(),
                            ir_else_block,
                        ),
                        result_type,
                    );
                    Ok(ir_expr)
                } else {
                    // no else
                    let unit_type = Ty::mk_unit(self.typing_cx);
                    if let Some(ty) = ir_then_block.ty(self.typing_cx) {
                        self.unify(subst, ty, unit_type)?;
                    }

                    // Convert condition to IR
                    subst.apply_substitution(self.typing_cx, &mut cond_ir.ty);

                    // Create IR if expression (without else)
                    let ir_expr = ir::Expr::new(
                        ast::ExprMut::if_then(
                            self.ir_cx,
                            if_expr.if_kw.into_token(),
                            cond_ir,
                            ir_then_block,
                        ),
                        unit_type,
                    );
                    Ok(ir_expr)
                }
            }
            ExprKind::Qualif(_) => Err(anyhow!("Qualif cannot be used as a standalone expression")),
            ExprKind::PreQualified(prequalified) => {
                if let ExprKind::Apply(apply) = &prequalified.expr.0 {
                    self.typeck_apply(env, subst, apply, prequalified.qualifs)
                } else {
                    Err(anyhow!(
                        "PreQualified expressions can only be applied to function calls"
                    ))
                }
            }
            ExprKind::Compare(compare) => {
                // Compare expressions have a head expression and a tail of (op, expr) pairs
                let mut head_ir = self.typeck_expr(env, subst, &compare.head)?;
                let mut ir_tail = Vec::new();

                // Type check all comparison operands - they should all have the same type
                let mut expected_type = head_ir.ty;

                for (op, expr) in compare.tail_with_op {
                    let mut expr_ir = self.typeck_expr(env, subst, expr)?;

                    // Unify with expected type
                    self.unify(subst, expected_type, expr_ir.ty)?;
                    subst.apply_substitution(self.typing_cx, &mut expr_ir.ty);
                    subst.apply_substitution(self.typing_cx, &mut expected_type);

                    // Apply final substitution to the expression
                    subst.apply_substitution(self.typing_cx, &mut expr_ir.ty);

                    let ty = expr_ir.ty;
                    ir_tail.push((op.into_token(), expr_ir));
                    expected_type = ty;
                }

                // Apply final substitution to head
                subst.apply_substitution(self.typing_cx, &mut head_ir.ty);

                // Create IR Compare expression - result is always bool
                let bool_type = Ty::mk_bool(self.typing_cx);
                let ir_expr = ir::Expr::new(
                    ast::ExprMut::compare(self.ir_cx, head_ir, ir_tail),
                    bool_type,
                );
                Ok(ir_expr)
            }
            ExprKind::Set(set) => {
                // Set expressions are assignment-like operations (lhs := rhs)
                let mut lhs_ir = self.typeck_expr(env, subst, &set.lhs)?;
                let mut rhs_ir = self.typeck_expr(env, subst, &set.rhs)?;

                // Unify lhs and rhs types - they should be the same
                self.unify(subst, lhs_ir.ty, rhs_ir.ty)?;
                // Apply final substitution to operands
                subst.apply_substitution(self.typing_cx, &mut lhs_ir.ty);
                subst.apply_substitution(self.typing_cx, &mut rhs_ir.ty);

                // Create IR Set expression - result is unit type
                let unit_type = Ty::mk_unit(self.typing_cx);
                let ir_expr = ir::Expr::new(
                    ast::ExprMut::set(self.ir_cx, lhs_ir, set.colon_eq.into_token(), rhs_ir),
                    unit_type,
                );
                Ok(ir_expr)
            }
            ExprKind::InfixImport(infix_import) => {
                // InfixImport expressions are like "file ? path" operations
                let mut file_ir = self.typeck_expr(env, subst, &infix_import.file)?;

                // File should be a string type
                let string_type = Ty::mk_string(self.typing_cx);
                self.unify(subst, file_ir.ty, string_type)?;

                // Apply final substitution to file
                subst.apply_substitution(self.typing_cx, &mut file_ir.ty);

                // Convert path to resolved path
                let ir_path = self.resolve_path(&infix_import.path)?;

                // InfixImport result type is that of the imported item
                let import_type = match self.module_loader.resolve_path(infix_import.path.raw) {
                    Some(item) => item.ty(),
                    None => {
                        return Err(anyhow!(
                            "Cannot resolve import path: {}",
                            infix_import.path.raw
                        ));
                    }
                };

                // Create IR InfixImport expression
                let ir_expr = ir::Expr::new(
                    ast::ExprMut::import(
                        self.ir_cx,
                        file_ir,
                        infix_import.question.into_token(),
                        ir_path,
                    ),
                    import_type,
                );
                Ok(ir_expr)
            }
        }
    }

    fn typeck_literal(
        &mut self,
        typing_env: &Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        literal: &'cx ast::Literal<'cx>,
    ) -> Result<ir::Expr<'cx>> {
        match literal {
            ast::Literal::String(s) => {
                let ir_string = ir::String {
                    value: self.ir_cx.alloc_str(&s.unescape()?),
                    syn: s,
                };
                let ir_literal = ast::Literal::String(ir_string);
                let string_type = Ty::mk_string(self.typing_cx);
                let ir_expr =
                    ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), string_type);
                Ok(ir_expr)
            }
            ast::Literal::Numeric(numeric) => {
                let numeric_kind = match &numeric.kind {
                    ast::literal::NumericKind::Integer(prefix) => {
                        // rawフィールドから実際の値を計算
                        let value = match numeric.raw.parse::<i64>() {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(anyhow!(
                                    "Failed to parse integer literal: {}",
                                    numeric.raw
                                ));
                            }
                        };
                        NumericKind::Int(*prefix, value)
                    }
                    ast::literal::NumericKind::Float => {
                        // フロートリテラルの値を計算
                        let value = match numeric.raw.parse::<f64>() {
                            Ok(v) => v,
                            Err(_) => {
                                return Err(anyhow!(
                                    "Failed to parse float literal: {}",
                                    numeric.raw
                                ));
                            }
                        };
                        NumericKind::Float(value)
                    }
                };
                let ir_numeric = ir::Numeric {
                    kind: numeric_kind,
                    syn: numeric,
                };
                let ir_literal = ast::Literal::Numeric(ir_numeric);
                let numeric_type = match numeric.kind {
                    ast::literal::NumericKind::Integer(_) => Ty::mk_int(self.typing_cx),
                    ast::literal::NumericKind::Float => Ty::mk_float(self.typing_cx),
                };
                let ir_expr =
                    ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), numeric_type);
                Ok(ir_expr)
            }
            ast::Literal::Array(array) => {
                if array.exprs.is_empty() {
                    // Empty array - use a type variable for the element type
                    let element_type = Ty::mk_variable(self.typing_cx, self.fresh_type_var());
                    let array_type = Ty::mk_array(self.typing_cx, element_type);

                    let ir_array = ast::literal::Array {
                        left_bracket: array.left_bracket.into_token(),
                        exprs: &[],
                        right_bracket: array.right_bracket.into_token(),
                    };
                    let ir_literal = ast::Literal::Array(ir_array);
                    let ir_expr =
                        ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), array_type);
                    Ok(ir_expr)
                } else {
                    // Non-empty array - type check all elements
                    let mut ir_exprs = Vec::new();

                    // Type check first element to establish the element type
                    let mut first_ir = self.typeck_expr(typing_env, subst, &array.exprs[0])?;

                    subst.apply_substitution(self.typing_cx, &mut first_ir.ty);
                    let ty = first_ir.ty;
                    ir_exprs.push(first_ir);
                    let mut element_type = ty;

                    // Type check remaining elements and unify with element type
                    for expr in &array.exprs[1..] {
                        let mut expr_ir = self.typeck_expr(typing_env, subst, expr)?;

                        self.unify(subst, element_type, expr_ir.ty)?;
                        subst.apply_substitution(self.typing_cx, &mut element_type);
                        subst.apply_substitution(self.typing_cx, &mut expr_ir.ty);

                        subst.apply_substitution(self.typing_cx, &mut expr_ir.ty);
                        let ty = expr_ir.ty;
                        ir_exprs.push(expr_ir);
                        element_type = ty;
                    }

                    let array_type = Ty::mk_array(self.typing_cx, element_type);
                    let ir_array = ast::Literal::array(
                        array.left_bracket.into_token(),
                        self.ir_cx.alloc_expr_slice(ir_exprs),
                        array.right_bracket.into_token(),
                    );
                    let ir_expr =
                        ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_array), array_type);
                    Ok(ir_expr)
                }
            }
            ast::Literal::Bytes(bytes) => {
                let byte_data = self.ir_cx.alloc_bytes(bytes.as_bytes());
                let ir_bytes = ir::Bytes {
                    value: byte_data,
                    syn: bytes,
                };
                let ir_literal = ast::Literal::Bytes(ir_bytes);
                let bytes_type = Ty::mk_string(self.typing_cx);
                let ir_expr =
                    ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), bytes_type);
                Ok(ir_expr)
            }
            ast::Literal::HexBytes(hex_bytes) => {
                let byte_data = self.ir_cx.alloc_bytes(
                    &hex_bytes
                        .as_bytes()
                        .map_err(|c| anyhow!("illegal hex charactor: {c}"))?,
                );
                let ir_hex_bytes = ir::HexBytes {
                    value: byte_data,
                    syn: hex_bytes,
                };
                let ir_literal = ast::Literal::HexBytes(ir_hex_bytes);
                let hex_bytes_type = Ty::mk_string(self.typing_cx);
                let ir_expr = ir::Expr::new(
                    ast::ExprMut::literal(self.ir_cx, ir_literal),
                    hex_bytes_type,
                );
                Ok(ir_expr)
            }
            ast::Literal::DateTime(dt) => {
                // Parse the raw datetime string to chrono::DateTime<Utc>
                let parsed_datetime = match dt.raw.parse::<chrono::DateTime<Utc>>() {
                    Ok(datetime) => datetime,
                    Err(_) => {
                        return Err(anyhow!("Failed to parse datetime literal: {}", dt.raw));
                    }
                };

                let ir_datetime = ir::DateTime {
                    value: parsed_datetime,
                    syn: dt,
                };
                let ir_literal = ast::Literal::DateTime(ir_datetime);
                let time_type = Ty::mk_time(self.typing_cx);
                let ir_expr =
                    ir::Expr::new(ast::ExprMut::literal(self.ir_cx, ir_literal), time_type);
                Ok(ir_expr)
            }
        }
    }

    fn resolve_ident(&self, ident: ast::Ident<'cx>) -> Result<Ident<'cx>> {
        let identifier = Identifier {
            name: ident.raw,
            scope_depth: 0, // 適切なスコープ深度の計算が必要
        };
        Ok(self.typing_cx.alloc_ident(identifier))
    }

    fn resolve_path(&self, path: &'cx ast::Path<'cx>) -> Result<ResolvedPath<'cx>> {
        match self.module_loader.resolve_path(path.raw) {
            Some(item) => Ok(ResolvedPath {
                item,
                original_path: path,
            }),
            None => Err(anyhow!("Cannot resolve path: {}", path.raw)),
        }
    }

    fn typeck_apply(
        &mut self,
        env: &Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        apply: &'cx ast::Apply<'cx>,
        qualifs: &'cx [ast::Qualif<'cx>],
    ) -> Result<ir::Expr<'cx>> {
        let func_ir = self.typeck_expr(env, subst, &apply.function)?;
        let mut arg_types = Vec::new();
        let mut args = Vec::new();
        let mut qualifications = Vec::new();

        // Process qualifications first
        for qualif in qualifs {
            let qualif_ir = self.typeck_qualif(env, subst, qualif)?;
            qualifications.push(qualif_ir);
        }

        // Process arguments, collecting any Qualif expressions
        for arg in apply.args {
            if let ExprKind::Qualif(qualif) = arg.0 {
                // Collect Qualif as qualification
                let qualif_ir = self.typeck_qualif(env, subst, qualif)?;
                qualifications.push(qualif_ir);
            } else {
                // Regular argument
                let mut arg_ir = self.typeck_expr(env, subst, arg)?;
                subst.apply_substitution(self.typing_cx, &mut arg_ir.ty);
                arg_types.push(arg_ir.ty);
                args.push(arg_ir);
            }
        }

        let return_type = Ty::mk_variable(self.typing_cx, self.fresh_type_var());
        let expected_func_type = Ty::mk_function(self.typing_cx, arg_types, return_type);

        self.unify(subst, func_ir.ty, expected_func_type)?;

        args.shrink_to_fit();
        qualifications.shrink_to_fit();

        // Create IR Apply expression
        let ir_apply = ir::Apply {
            function: func_ir,
            args,
            qualifications,
            resolved_function: None, // FIXME
        };
        let ir_expr_kind = ExprKind::Apply(ir_apply);
        let ir_expr = self.ir_cx.alloc_expr_with_type(ir_expr_kind, return_type);
        Ok(ir_expr)
    }

    fn typeck_qualif(
        &mut self,
        env: &Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        qualif: &'cx ast::Qualif<'cx>,
    ) -> Result<ast::Qualif<'cx, IrTypeFamily>> {
        match qualif {
            ast::Qualif::Modifier(modifier) => {
                let ir_path = self.resolve_path(&modifier.id)?;
                let ir_param = if let Some(param) = &modifier.arg {
                    let param_ir = self.typeck_expr(env, subst, &param.value)?;
                    Some(ast::ModifierParam {
                        colon_token: param.colon_token.into_token(),
                        value: param_ir,
                    })
                } else {
                    None
                };

                Ok(ast::Qualif::Modifier(ast::Modifier {
                    at_token: modifier.at_token.into_token(),
                    id: ir_path,
                    arg: ir_param,
                }))
            }
            ast::Qualif::DefaultModifier(default_modifier) => {
                let ir_path = self.resolve_path(&default_modifier.value)?;
                Ok(ast::Qualif::DefaultModifier(ast::DefaultModifier {
                    tilde_token: default_modifier.tilde_token.into_token(),
                    value: ir_path,
                }))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typing_context() {
        let cx = TypingContext::new();

        let int_type = Ty::mk_int(&cx);
        let array_type = Ty::mk_array(&cx, int_type);

        assert_eq!(cx.display_type(int_type), "i32");
        assert_eq!(cx.display_type(array_type), "[i32]");

        let identifier = Identifier {
            name: "test",
            scope_depth: 0,
        };
        let id = cx.alloc_ident(identifier);
        assert_eq!(id.name, "test");
    }

    #[test]
    fn test_builtin_module() {
        let cx = TypingContext::new();
        let builtin = create_builtin_module(&cx);

        assert_eq!(builtin.name(), "builtin");
        assert!(builtin.lookup_item("i32").is_some());
        assert!(builtin.lookup_item("f64").is_some());
        assert!(builtin.lookup_item("unknown").is_none());

        if let Some(ModuleItem::Type { id, ty }) = builtin.lookup_item("i32") {
            assert_eq!(id.name, "i32");
            assert!(matches!(ty.kind(), TyKind::Int));
        }
    }

    #[test]
    fn test_module_loader() {
        let cx = TypingContext::new();
        let builtin = create_builtin_module(&cx);

        let mut loader = ModuleLoader::new();
        loader.add_module(builtin);

        assert!(loader.lookup_module("builtin").is_some());
        assert!(loader.lookup_module("unknown").is_none());

        assert!(loader.resolve_path("i32").is_some());
        assert!(loader.resolve_path("unknown").is_none());
    }

    #[test]
    fn test_type_checker_with_modules() {
        let cx = TypingContext::new();
        let ir_cx = ir::Context::new();
        let builtin = create_builtin_module(&cx);

        let mut loader = ModuleLoader::new();
        loader.add_module(builtin);

        let checker = TypeChecker::with_module_loader(loader, &cx, &ir_cx);

        let i32_type = checker.resolve_type_from_path("i32").unwrap();
        assert!(matches!(i32_type.kind(), TyKind::Int));

        let unknown_result = checker.resolve_type_from_path("unknown");
        assert!(unknown_result.is_err());
    }

    #[test]
    fn test_substitution() {
        let cx = TypingContext::new();
        let var = TypeVariable::fresh();
        let int_type = Ty::mk_int(&cx);

        let mut subst = Substitution::new();
        subst.insert(var, int_type);

        let var_type = Ty::mk_variable(&cx, var);
        let result = subst.apply_substitution_pure(&cx, var_type);

        assert!(matches!(result.kind(), TyKind::Int));
    }

    #[test]
    fn test_unify_basic() {
        let cx = TypingContext::new();
        let ir_cx = ir::Context::new();

        let int_type1 = Ty::mk_int(&cx);
        let int_type2 = Ty::mk_int(&cx);
        let float_type = Ty::mk_float(&cx);

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
        let local_identifier = Identifier {
            name: "i32",
            scope_depth: 0,
        };
        let local_id = cx.alloc_ident(local_identifier);
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
