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
                    let resolved_ident = self.resolve_ident(ident)?;
                    let resolved_path = ir::ResolvedPath {
                        item: ir::ResolvedItem::LocalVariable(resolved_ident),
                        original_path: path,
                    };

                    let ir_expr =
                        ir::Expr::new(ast::ExprMut::variable(self.ir_cx, resolved_path), type_ref);
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
                        ir::Expr::new(ast::ExprMut::variable(self.ir_cx, resolved_path), *ty);
                    Ok(ir_expr)
                } else {
                    Err(anyhow!("unbound variable: {path}"))
                }
            }
            ast::ExprKind::Binary(binary) => {
                let lhs_ir = self.typeck_expr(env, subst, &binary.lhs)?;
                let rhs_ir = self.typeck_expr(env, subst, &binary.rhs)?;

                match binary.op {
                    ast::BinOp::Add(_)
                    | ast::BinOp::Sub(_)
                    | ast::BinOp::Mul(_)
                    | ast::BinOp::Div(_) => {
                        self.unify(subst, lhs_ir.ty, rhs_ir.ty)?;
                        match lhs_ir.ty.kind() {
                            TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_) => {
                                // Apply final substitution to operands

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
                                "arithmetic operation requires numeric type, got {}",
                                lhs_ir.ty.display()
                            )),
                        }
                    }
                    ast::BinOp::Mod(_) => {
                        self.unify(subst, lhs_ir.ty, rhs_ir.ty)?;
                        match lhs_ir.ty.kind() {
                            TyKind::Int(_) | TyKind::Uint(_) => {
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
                                "modulo operation requires integer type, got {}",
                                lhs_ir.ty.display()
                            )),
                        }
                    }
                    ast::BinOp::And(_) | ast::BinOp::Or(_) => {
                        let bool_type = Ty::mk_bool(self.typing_cx);
                        self.unify(subst, lhs_ir.ty, bool_type)?;
                        self.unify(subst, lhs_ir.ty, bool_type)?;

                        // Apply final substitution to operands

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
                                lhs_ir.ty.display()
                            )),
                        }
                    }
                }
            }
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
                                self.unify(subst, expr_ir.ty, Ty::mk_i32(self.typing_cx))?
                            }
                        }
                        // Create IR operand with unified type

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
            ast::ExprKind::Apply(apply) => self.typeck_apply(env, subst, apply, &[]),
            ast::ExprKind::If(if_expr) => {
                let cond_ir = self.typeck_expr(env, subst, &if_expr.cond)?;
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
                let mut expected_type = head_ir.ty;

                for (op, expr) in compare.tail_with_op {
                    let expr_ir = self.typeck_expr(env, subst, expr)?;

                    // Unify with expected type
                    self.unify(subst, expected_type, expr_ir.ty)?;

                    // Apply final substitution to the expression

                    let ty = expr_ir.ty;
                    ir_tail.push((op.into_token(), expr_ir));
                    expected_type = ty;
                }

                // Apply final substitution to head

                // Create IR Compare expression - result is always bool
                let bool_type = Ty::mk_bool(self.typing_cx);
                let ir_expr = ir::Expr::new(
                    ast::ExprMut::compare(self.ir_cx, head_ir, ir_tail),
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
                // Apply final substitution to operands

                // Create IR Set expression - result is unit type
                let unit_type = Ty::mk_unit(self.typing_cx);
                let ir_expr = ir::Expr::new(
                    ast::ExprMut::set(self.ir_cx, lhs_ir, set.colon_eq.into_token(), rhs_ir),
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
                let string_type = Ty::mk_string(self.typing_cx);
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
                    ast::ExprMut::import(
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
