use super::*;

impl<'cx> TypeChecker<'cx> {
    /// Performs type checking on an expression and converts it to IR.
    ///
    /// This function infers the type of an expression and converts it to its IR representation.
    /// It updates the provided substitution with any new type constraints discovered during checking.
    pub(super) fn typeck_binary<'env>(
        &mut self,
        session: &Session<'cx, 'env>,
        env: &Scope<'cx, 'env>,
        subst: &mut Substitution<'cx>,
        expr: &'cx ast::Binary<'cx>,
    ) -> Result<ir::Expr<'cx>> {
        let binary = expr;
        let mut lhs_ir = self.typeck_expr(session, env, subst, &binary.lhs)?;
        let mut rhs_ir = self.typeck_expr(session, env, subst, &binary.rhs)?;

        match binary.op {
            ast::BinOp::Add(span) => {
                self.eagerly_resolve(subst, &mut lhs_ir.ty)?;
                self.eagerly_resolve(subst, &mut rhs_ir.ty)?;
                self.typeck_add_sub(lhs_ir, rhs_ir, |operand| ir::BinOp::Add {
                    kind: ir::BuiltinAdd::Basic(operand),
                    span: span.into_token(),
                })
            }
            ast::BinOp::Sub(span) => {
                self.eagerly_resolve(subst, &mut lhs_ir.ty)?;
                self.eagerly_resolve(subst, &mut rhs_ir.ty)?;
                self.typeck_add_sub(lhs_ir, rhs_ir, |operand| ir::BinOp::Sub {
                    kind: ir::BuiltinSub::Basic(operand),
                    span: span.into_token(),
                })
            }
            ast::BinOp::Mul(span) => {
                self.eagerly_resolve(subst, &mut lhs_ir.ty)?;
                self.eagerly_resolve(subst, &mut rhs_ir.ty)?;
                self.typeck_mul_div(lhs_ir, rhs_ir, |operand| ir::BinOp::Mul {
                    kind: ir::BuiltinMul::Basic(operand),
                    span: span.into_token(),
                })
            }
            ast::BinOp::Div(span) => {
                self.eagerly_resolve(subst, &mut lhs_ir.ty)?;
                self.eagerly_resolve(subst, &mut rhs_ir.ty)?;
                self.typeck_mul_div(lhs_ir, rhs_ir, |operand| ir::BinOp::Div {
                    kind: ir::BuiltinDiv::Basic(operand),
                    span: span.into_token(),
                })
            }
            ast::BinOp::Mod(span) => {
                self.unify(subst, lhs_ir.ty, rhs_ir.ty)?;
                if lhs_ir.ty.is_integer() {
                    // Create IR binary expression
                    let ty = lhs_ir.ty;
                    let ir_expr = ir::Expr::new(
                        ir::ExprMut::binary(
                            self.ir_cx,
                            lhs_ir,
                            ir::BinOp::Mod {
                                span: span.into_token(),
                            },
                            rhs_ir,
                        ),
                        ty,
                    );
                    Ok(ir_expr)
                } else {
                    Err(anyhow!(
                        "modulo operation requires integer type, got {}",
                        lhs_ir.ty
                    ))
                }
            }
            ast::BinOp::And(span) => {
                let bool_type = Ty::mk_bool(self.tcx);
                self.unify(subst, lhs_ir.ty, bool_type)?;
                self.unify(subst, lhs_ir.ty, bool_type)?;

                // Create IR binary expression
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(
                        self.ir_cx,
                        lhs_ir,
                        ir::BinOp::And {
                            span: span.into_token(),
                        },
                        rhs_ir,
                    ),
                    bool_type,
                );
                Ok(ir_expr)
            }
            ast::BinOp::Or(span) => {
                let bool_type = Ty::mk_bool(self.tcx);
                self.unify(subst, lhs_ir.ty, bool_type)?;
                self.unify(subst, lhs_ir.ty, bool_type)?;

                // Create IR binary expression
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(
                        self.ir_cx,
                        lhs_ir,
                        ir::BinOp::Or {
                            span: span.into_token(),
                        },
                        rhs_ir,
                    ),
                    bool_type,
                );
                Ok(ir_expr)
            }
        }
    }

    fn typeck_add_sub(
        &mut self,
        lhs_ir: ir::Expr<'cx>,
        rhs_ir: ir::Expr<'cx>,
        op: impl FnOnce(ir::BuiltinAddSubOperand) -> ir::BinOp<'cx>,
    ) -> Result<ir::Expr<'cx>> {
        match (lhs_ir.ty.kind(), rhs_ir.ty.kind()) {
            (&TyKind::Int(lhs_ty), &TyKind::Int(rhs_ty)) => {
                let result_ty = lhs_ty.max(rhs_ty);
                let operand = ir::BuiltinAddSubOperand::Int(ir::MagmaTriad {
                    lhs_ty,
                    rhs_ty,
                    result_ty,
                });
                let ty = Ty::mk_int(self.tcx, result_ty);
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            (&TyKind::Uint(lhs_ty), &TyKind::Uint(rhs_ty)) => {
                let result_ty = lhs_ty.max(rhs_ty);
                let operand = ir::BuiltinAddSubOperand::Uint(ir::MagmaTriad {
                    lhs_ty,
                    rhs_ty,
                    result_ty,
                });
                let ty = Ty::mk_uint(self.tcx, result_ty);
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            (&TyKind::Float(lhs_ty), &TyKind::Float(rhs_ty)) => {
                let result_ty = lhs_ty.max(rhs_ty);
                let operand = ir::BuiltinAddSubOperand::Float(ir::MagmaTriad {
                    lhs_ty,
                    rhs_ty,
                    result_ty,
                });
                let ty = Ty::mk_float(self.tcx, result_ty);
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            (&TyKind::Duration, &TyKind::Duration) => {
                let ty = Ty::mk_duration(self.tcx);
                let operand = ir::BuiltinAddSubOperand::DurationDuration;
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            (&TyKind::Time, &TyKind::Duration) => {
                let ty = Ty::mk_time(self.tcx);
                let operand = ir::BuiltinAddSubOperand::DateTimeDuration;
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            (&TyKind::Duration, &TyKind::Time) => {
                let ty = Ty::mk_time(self.tcx);
                let operand = ir::BuiltinAddSubOperand::DurationDateTime;
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            _ => Err(anyhow!(
                "addition requires both operands to be of compatible types (int, uint, float, duration, time), got {} and {}",
                lhs_ir.ty,
                rhs_ir.ty
            )),
        }
    }

    fn typeck_mul_div(
        &mut self,
        lhs_ir: ir::Expr<'cx>,
        rhs_ir: ir::Expr<'cx>,
        op: impl FnOnce(ir::BuiltinMulDivOperand) -> ir::BinOp<'cx>,
    ) -> Result<ir::Expr<'cx>> {
        match (lhs_ir.ty.kind(), rhs_ir.ty.kind()) {
            (&TyKind::Int(lhs_ty), &TyKind::Int(rhs_ty)) => {
                let result_ty = lhs_ty.max(rhs_ty);
                let operand = ir::BuiltinMulDivOperand::Int(ir::MagmaTriad {
                    lhs_ty,
                    rhs_ty,
                    result_ty,
                });
                let ty = Ty::mk_int(self.tcx, result_ty);
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            (&TyKind::Uint(lhs_ty), &TyKind::Uint(rhs_ty)) => {
                let result_ty = lhs_ty.max(rhs_ty);
                let operand = ir::BuiltinMulDivOperand::Uint(ir::MagmaTriad {
                    lhs_ty,
                    rhs_ty,
                    result_ty,
                });
                let ty = Ty::mk_uint(self.tcx, result_ty);
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            (&TyKind::Float(lhs_ty), &TyKind::Float(rhs_ty)) => {
                let result_ty = lhs_ty.max(rhs_ty);
                let operand = ir::BuiltinMulDivOperand::Float(ir::MagmaTriad {
                    lhs_ty,
                    rhs_ty,
                    result_ty,
                });
                let ty = Ty::mk_float(self.tcx, result_ty);
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            (&TyKind::Duration, rhs @ TyKind::Int(_))
            | (&TyKind::Duration, rhs @ TyKind::Uint(_))
            | (&TyKind::Duration, rhs @ TyKind::Float(_)) => {
                let rhs_ty = match rhs {
                    TyKind::Int(ty) => ty::Numeric::Int(*ty),
                    TyKind::Uint(ty) => ty::Numeric::Uint(*ty),
                    TyKind::Float(ty) => ty::Numeric::Float(*ty),
                    _ => unreachable!(),
                };
                let operand = ir::BuiltinMulDivOperand::DurationNumeric(rhs_ty);
                let ty = Ty::mk_duration(self.tcx);
                let ir_expr = ir::Expr::new(
                    ir::ExprMut::binary(self.ir_cx, lhs_ir, op(operand), rhs_ir),
                    ty,
                );
                Ok(ir_expr)
            }
            _ => Err(anyhow!(
                "multiplication/division requires both operands to be of compatible types (int, uint, float, duration), got {} and {}",
                lhs_ir.ty,
                rhs_ir.ty
            )),
        }
    }
}
