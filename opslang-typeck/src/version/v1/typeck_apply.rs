use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn typeck_apply(
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
                let arg_ir = self.typeck_expr(env, subst, arg)?;
                arg_types.push(arg_ir.ty);
                args.push(arg_ir);
            }
        }

        let return_type = Ty::mk_variable(self.typing_cx, TypeVariable::fresh());
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
                let ir_path = self.resolve_path(&modifier.id)?.resolved_path;
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
                let ir_path = self.resolve_path(&default_modifier.value)?.resolved_path;
                Ok(ast::Qualif::DefaultModifier(ast::DefaultModifier {
                    tilde_token: default_modifier.tilde_token.into_token(),
                    value: ir_path,
                }))
            }
        }
    }
}
