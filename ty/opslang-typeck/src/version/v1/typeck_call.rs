use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn typeck_call<'env>(
        &mut self,
        env: &Environment<'cx, 'env>,
        subst: &mut Substitution<'cx>,
        call: &'cx ast::Call<'cx>,
    ) -> Result<ir::Expr<'cx>> {
        if let ast::ExprKind::Apply(apply) = call.expr.0 {
            let func_ir = self.typeck_expr(env, subst, &apply.function)?;
            let mut arg_types = Vec::new();
            let mut args = Vec::new();

            // Process arguments
            for arg in apply.args {
                let arg_ir = self.typeck_expr(env, subst, arg)?;
                arg_types.push(arg_ir.ty);
                args.push(arg_ir);
            }

            let return_type = Ty::mk_variable(self.tcx, TyVid::fresh());
            let expected_func_type = Ty::mk_function(self.tcx, arg_types, return_type);

            self.unify(subst, func_ir.ty, expected_func_type)?;

            guard_calling_non_prc(&func_ir)?;

            args.shrink_to_fit();

            // Create IR Apply expression
            let ir_apply = ir::Apply {
                function: func_ir,
                args,
                qualifications: vec![],
                resolved_function: None, // FIXME
            };
            let ir_expr_kind = ir::ExprKind::Apply(ir_apply);
            let ir_expr = self.ir_cx.alloc_expr_with_type(ir_expr_kind, return_type);
            Ok(ir_expr)
        } else {
            // Allow calling `() -> T` procedures
            let func_ir = self.typeck_expr(env, subst, call.expr.0)?;
            let return_type = Ty::mk_variable(self.tcx, TyVid::fresh());
            let expected_func_type = Ty::mk_function(self.tcx, vec![], return_type);
            self.unify(subst, func_ir.ty, expected_func_type)?;
            guard_calling_non_prc(&func_ir)?;
            let ir_apply = ir::Apply {
                function: func_ir,
                args: vec![],
                qualifications: vec![],
                resolved_function: None, // FIXME
            };
            let ir_expr_kind = ir::ExprKind::Apply(ir_apply);
            let ir_expr = self.ir_cx.alloc_expr_with_type(ir_expr_kind, return_type);
            Ok(ir_expr)
        }
    }
}

/// Ensures that the function being called is a procedure.
fn guard_calling_non_prc<'cx>(func_ir: &ir::Expr<'cx>) -> Result<()> {
    if let TyKind::Function {
        arg: _,
        ret: _,
        is_procedure,
    } = func_ir.ty.kind()
    {
        if is_procedure.is_none() {
            return Err(anyhow!(
                "cannot call a pure function, remove `call` before the function name"
            ));
        }
    } else {
        unreachable!();
    }
    Ok(())
}
