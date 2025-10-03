use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn typeck_function(
        &mut self,
        global_env: &Environment<'cx, '_>,
        func_def: &'cx ast::FunctionDef<'cx>,
    ) -> Result<ir::FunctionDef<'cx>> {
        let func_name = func_def.name;

        // Retrieve the already resolved function type from the environment
        let environment::TypedIdent {
            id: typed_func_name,
            ty: func_type,
        } = global_env
            .lookup_var(func_name)
            .ok_or_else(|| anyhow!("function '{func_name}' not found in environment"))?;

        let TyKind::Function {
            arg: param_types,
            ret: return_type,
            is_procedure: _,
        } = func_type.kind()
        else {
            return Err(anyhow!("expected function type for '{func_name}'"));
        };

        let mut func_env = global_env.extend_inherit();

        // Convert parameters to IR using the already resolved types
        let mut ir_parameters = Vec::new();
        for (param, param_type) in func_def.parameters.iter().zip(param_types) {
            let param_id = self.bind(&mut func_env, param.name, *param_type);

            let ir_param = ir::Parameter {
                name: param_id,
                colon: param.colon.into_token(),
                ty: self.resolve_type_from_path(param.ty)?,
            };
            ir_parameters.push(ir_param);
        }

        // Type check function body and collect substitutions
        let mut subst = Substitution::new();
        let mut ir_body = self.typeck_block(&func_env, &mut subst, func_def.body)?;

        // Apply final substitution using visitor
        hm::SubstitutionVisitor::new(subst, self.tcx).visit_mut(&mut ir_body);

        Ok(ir::FunctionDef {
            prc_token: func_def.prc_token.into_token(),
            name: typed_func_name,
            left_paren: func_def.left_paren.into_token(),
            parameters: self.ir_cx.alloc_parameter_slice(ir_parameters),
            right_paren: func_def.right_paren.into_token(),
            return_type: *return_type,
            body: ir_body,
        })
    }
}
