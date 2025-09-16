use super::*;

impl<'cx> TypeChecker<'cx> {
    /// Performs type checking on an expression and converts it to IR.
    ///
    /// This function infers the type of an expression and converts it to its IR representation.
    /// It updates the provided substitution with any new type constraints discovered during checking.
    pub(super) fn typeck_expr(
        &mut self,
        env: &Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        mut expr: &'cx ast::ExprKind<'cx>,
    ) -> Result<ir::Expr<'cx>> {
        // Peel parentheses
        while let ast::ExprKind::Parened(parened) = expr {
            expr = &parened.expr;
        }
        match expr {
            ast::ExprKind::Parened(_parened) => unreachable!("handled above"),
            ast::ExprKind::Literal(literal) => self.typeck_literal(env, subst, literal),
            ast::ExprKind::Variable(path) => {
                // First check if this is a single identifier that can be resolved in local environment
                if let Some(ident) = path.is_ident()
                    && let Some(type_ref) = env.lookup_variable(ident)
                {
                    // Found in local environment - create a resolved path with local variable
                    let resolved_ident = self.tcx.alloc_identifier(ident.raw);
                    let resolved_path = ir::ResolvedPath {
                        item: ir::ResolvedItem::LocalVariable(resolved_ident),
                        original_path: path,
                    };

                    let ir_expr =
                        ir::Expr::new(ir::ExprMut::variable(self.ir_cx, resolved_path), type_ref);
                    return Ok(ir_expr);
                }

                // Fall back to module resolution
                if let Ok(ResolvePathResult {
                    resolved_path,
                    item,
                }) = self.resolve_path(path)
                    && let ModuleItem::Constant { ty, .. } | ModuleItem::Prc { ty, .. } = item
                {
                    let ir_expr =
                        ir::Expr::new(ir::ExprMut::variable(self.ir_cx, resolved_path), *ty);
                    return Ok(ir_expr);
                }

                // Try external resolver if available
                if let Some(ty) = self.try_external_resolve(*path) {
                    let resolved_path = ir::ResolvedPath {
                        item: ir::ResolvedItem::External,
                        original_path: path,
                    };
                    let wrapped_ty = Ty::mk_external(self.tcx, *path, ty);
                    let ir_expr =
                        ir::Expr::new(ir::ExprMut::variable(self.ir_cx, resolved_path), wrapped_ty);
                    return Ok(ir_expr);
                }

                // Not found
                Err(anyhow!("unbound variable: {path}"))
            }
            ast::ExprKind::Binary(binary) => self.typeck_binary(env, subst, binary),
            ast::ExprKind::Unary(unary) => {
                let expr_ir = self.typeck_expr(env, subst, &unary.expr)?;

                match unary.op {
                    ast::UnOp::Neg(_) => {
                        match expr_ir.ty.kind() {
                            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_) => {
                                // Ok, do nothing
                            }
                            _ => {
                                // resolve to default int
                                self.unify(subst, expr_ir.ty, Ty::mk_i32(self.tcx))?
                            }
                        }
                        // Create IR operand with unified type

                        // Create IR unary expression
                        let ty = expr_ir.ty;
                        let ir_result = ir::Expr::new(
                            ir::ExprMut::unary(self.ir_cx, unary.op.into_token(), expr_ir),
                            ty,
                        );
                        Ok(ir_result)
                    }
                    ast::UnOp::IdRef(_) => {
                        // IdRef (&expr) - creates a reference to the expression
                        // For now, we'll implement this as a simple unary operation
                        // The type system may need extension for proper reference types

                        // Create IR unary expression - result type is the same for now
                        let ty = expr_ir.ty;
                        let ir_result = ir::Expr::new(
                            ir::ExprMut::unary(self.ir_cx, unary.op.into_token(), expr_ir),
                            ty,
                        );
                        Ok(ir_result)
                    }
                    ast::UnOp::Deref(_) => {
                        // Deref ($expr) - dereferences a reference
                        // For now, we'll implement this as a simple unary operation
                        // The type system may need extension for proper reference types
                        // Create IR unary expression - result type is the same for now
                        let ty = expr_ir.ty;
                        let ir_result = ir::Expr::new(
                            ir::ExprMut::unary(self.ir_cx, unary.op.into_token(), expr_ir),
                            ty,
                        );
                        Ok(ir_result)
                    }
                }
            }
            ast::ExprKind::Apply(apply) => self.typeck_apply(env, subst, apply, &[]),
            ast::ExprKind::If(if_expr) => {
                let cond_ir = self.typeck_expr(env, subst, &if_expr.cond)?;
                let bool_type = Ty::mk_bool(self.tcx);
                self.unify(subst, cond_ir.ty, bool_type)?;
                let ir_then_block = self.typeck_block(env, subst, if_expr.then_clause)?;

                if let Some(else_clause) = &if_expr.else_opt {
                    let ir_else_block = self.typeck_block(env, subst, else_clause.else_clause)?;
                    let unified_then = subst.apply_substitution_pure(
                        self.tcx,
                        ir_then_block.ty(self.tcx).unwrap_or(Ty::mk_unit(self.tcx)),
                    );
                    let unified_else = subst.apply_substitution_pure(
                        self.tcx,
                        ir_else_block.ty(self.tcx).unwrap_or(Ty::mk_unit(self.tcx)),
                    );
                    self.unify(subst, unified_then, unified_else)?;
                    let result_type = subst.apply_substitution_pure(self.tcx, unified_then);

                    // Create IR if-else expression
                    let ir_expr = ir::Expr::new(
                        ir::ExprMut::if_then_else(
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
                    let unit_type = Ty::mk_unit(self.tcx);
                    if let Some(ty) = ir_then_block.ty(self.tcx) {
                        self.unify(subst, ty, unit_type)?;
                    }

                    // Create IR if expression (without else)
                    let ir_expr = ir::Expr::new(
                        ir::ExprMut::if_then(
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
            ast::ExprKind::Select(select) => {
                let ast::Select {
                    select_kw,
                    left_brace,
                    items,
                    right_brace,
                } = select;
                let mut ir_items = Vec::new();
                let mut result_type = None;

                for item in *items {
                    let mut expr_ir = self.typeck_expr(env, subst, &item.expr)?;
                    let body_ir = self.typeck_block(env, subst, item.body)?;

                    // Check that the expression is awaitable
                    self.eagerly_resolve(subst, &mut expr_ir.ty);
                    let ty = expr_ir.ty;
                    let is_awaitable = ty.is_bool() || ty.is_duration();
                    if !is_awaitable {
                        return Err(anyhow!(
                            "select expression requires awaitable type (bool or duration), got {ty}",
                        ));
                    }

                    // Ensure all bodies have the same type
                    if let Some(existing_type) = result_type {
                        let body_type = body_ir.ty(self.tcx).unwrap_or(Ty::mk_unit(self.tcx));
                        self.unify(subst, existing_type, body_type)?;
                        result_type = Some(subst.apply_substitution_pure(self.tcx, existing_type));
                    } else {
                        result_type = Some(body_ir.ty(self.tcx).unwrap_or(Ty::mk_unit(self.tcx)));
                    }

                    ir_items.push(ir::SelectItem {
                        expr: expr_ir,
                        arrow: item.arrow.into_token(),
                        body: body_ir,
                    });
                }

                let final_type = result_type.unwrap_or(Ty::mk_unit(self.tcx));

                // Create IR select expression
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::select(
                        self.ir_cx,
                        select_kw.into_token(),
                        left_brace.into_token(),
                        ir_items,
                        right_brace.into_token(),
                    ),
                    final_type,
                );
                Ok(ir_expr)
            }
            ast::ExprKind::Qualif(_) => {
                Err(anyhow!("Qualif cannot be used as a standalone expression"))
            }
            ast::ExprKind::PreQualified(prequalified) => {
                if let ast::ExprKind::Apply(apply) = &prequalified.expr.0 {
                    self.typeck_apply(env, subst, apply, prequalified.qualifs)
                } else {
                    Err(anyhow!(
                        "PreQualified expressions can only be applied to function calls"
                    ))
                }
            }
            ast::ExprKind::Compare(compare) => {
                // Compare expressions have a head expression and a tail of (op, expr) pairs
                let head_ir = self.typeck_expr(env, subst, &compare.head)?;
                let mut ir_tail = Vec::new();

                // Type check all comparison operands - they should all have the same type
                let expected_type = head_ir.ty;

                for (op, expr) in compare.tail_with_op {
                    let expr_ir = self.typeck_expr(env, subst, expr)?;

                    // Unify with expected type
                    self.unify(subst, expected_type, expr_ir.ty)?;

                    ir_tail.push((op.into_token(), expr_ir));
                }

                // Create IR Compare expression - result is always bool
                let bool_type = Ty::mk_bool(self.tcx);
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::compare(self.ir_cx, head_ir, ir_tail),
                    bool_type,
                );
                Ok(ir_expr)
            }
            ast::ExprKind::Set(set) => {
                // Set expressions are assignment-like operations (lhs := rhs)
                let lhs_ir = self.typeck_expr(env, subst, &set.lhs)?;
                let rhs_ir = self.typeck_expr(env, subst, &set.rhs)?;

                // Unify lhs and rhs types - they should be the same
                self.unify(subst, lhs_ir.ty, rhs_ir.ty)?;

                // Create IR Set expression - result is unit type
                let unit_type = Ty::mk_unit(self.tcx);
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::set(self.ir_cx, lhs_ir, set.colon_eq.into_token(), rhs_ir),
                    unit_type,
                );
                Ok(ir_expr)
            }
            ast::ExprKind::InfixImport(infix_import) => {
                let ast::InfixImport {
                    file,
                    question,
                    path,
                } = infix_import;
                // InfixImport expressions are like "file ? path" operations
                let file_ir = self.typeck_expr(env, subst, file)?;

                // File should be a string type
                let string_type = Ty::mk_string(self.tcx);
                self.unify(subst, file_ir.ty, string_type)?;

                // FIXME: Resolve path in loaded file
                let resolved = self.resolve_path(path)?;

                let (ModuleItem::Constant { ty, .. } | ModuleItem::Prc { ty, .. }) = resolved.item
                else {
                    // FIXME: display Expr
                    return Err(anyhow!("`{path}` is not a member of given file"));
                };

                // Create IR InfixImport expression
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::import(
                        self.ir_cx,
                        file_ir,
                        question.into_token(),
                        resolved.resolved_path,
                    ),
                    *ty,
                );
                Ok(ir_expr)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_external_resolve() {
        struct Resolver;
        impl<'cx> ExternalResolver<'cx> for Resolver {
            // A simple resolver that maps all variable to i32 type for testing
            fn resolve(
                &self,
                _path: ast::Path<'cx>,
                cx: &'cx TypingContext<'cx>,
            ) -> Option<Ty<'cx>> {
                Some(Ty::mk_i32(cx))
            }
        }

        let tcx = TypingContext::new();
        let ast_cx = ast::context::Context::new();
        let ir_cx = ir::Context::new();
        let mut type_checker = TypeChecker::new(&tcx, &ir_cx);
        type_checker.add_external_resolver(Resolver);
        let path = ast::Path::single(
            &ast_cx,
            "ExternalVar",
            ast::Span {
                start: ast::BytePos(0),
                end: ast::BytePos(11),
            },
        );
        let resolved_ty = type_checker.try_external_resolve(path);
        assert_eq!(resolved_ty, Some(Ty::mk_i32(&tcx)));

        let expr = ast_cx.alloc_expr(ast::ExprKind::Variable(path));
        let mut subst = Substitution::new();
        let env = Environment::new();
        let ir_expr = type_checker.typeck_expr(&env, &mut subst, &expr).unwrap();
        assert_eq!(ir_expr.ty, Ty::mk_external(&tcx, path, Ty::mk_i32(&tcx)));
    }
}
