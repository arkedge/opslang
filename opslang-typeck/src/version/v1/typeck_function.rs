use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn typeck_function(
        &mut self,
        global_env: &Environment<'cx, '_>,
        func_def: &'cx ast::FunctionDef<'cx>,
    ) -> Result<ast::FunctionDef<'cx, IrTypeFamily>> {
        let func_name = func_def.name;

        // Retrieve the already resolved function type from the environment
        let func_type = global_env
            .lookup_variable(func_name)
            .ok_or_else(|| anyhow!("function '{func_name}' not found in environment"))?;

        let (param_types, return_type) = match func_type.kind() {
            TyKind::Function { arg, ret } => (arg.clone(), *ret),
            _ => return Err(anyhow!("expected function type for '{func_name}'")),
        };

        let mut func_env = global_env.extend_inherit();

        // Convert parameters to IR using the already resolved types
        let mut ir_parameters = Vec::new();
        for (param, param_type) in func_def.parameters.iter().zip(param_types.iter()) {
            let param_name = param.name.raw;

            let param_identifier_id = self.typing_cx.alloc_identifier(param_name);
            func_env.bind(param_name, param_identifier_id, *param_type);

            let ir_param = ast::Parameter {
                name: param_identifier_id,
                colon: param.colon.into_token(),
                ty: self.resolve_path(&param.ty)?.item.ty,
            };
            ir_parameters.push(ir_param);
        }

        // Type check function body and collect substitutions
        let mut subst = Substitution::new();
        let mut ir_body = self.typeck_block(&func_env, &mut subst, func_def.body)?;

        // Apply final substitution using visitor
        let mut visitor = SubstitutionVisitor::new(subst, self.typing_cx);
        visitor.visit_mut(&mut ir_body);

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
}
