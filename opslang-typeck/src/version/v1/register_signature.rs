use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn register_definition(
        &mut self,
        env: &mut Environment<'cx, '_>,
        definition: &ast::ToplevelItem<'cx>,
    ) -> Result<()> {
        if let Some(kind) = &definition.kind {
            match kind {
                ast::DefinitionKind::Function(func_def) => {
                    self.register_function_signature(env, func_def)?;
                }
                ast::DefinitionKind::Constant(const_def) => {
                    self.register_constant_signature(env, const_def)?;
                }
            }
        }
        Ok(())
    }

    fn register_function_signature(
        &mut self,
        env: &mut Environment<'cx, '_>,
        func_def: &ast::FunctionDef<'cx>,
    ) -> Result<()> {
        let func_name = func_def.name;

        // Check if function with same name already exists
        if env.lookup_var(func_name).is_some() {
            return Err(anyhow!("function '{func_name}' is already defined"));
        }

        let mut param_types = Vec::new();

        for param in func_def.parameters {
            let param_type = self.resolve_type_from_path(param.ty)?;
            param_types.push(param_type);
        }

        // Handle return type from function definition
        let return_type = match &func_def.return_type.0 {
            Some((_, return_path)) => self.resolve_type_from_path(*return_path)?,
            None => Ty::mk_unit(self.tcx),
        };

        let func_identifier_id = self.tcx.alloc_identifier(func_name);

        let func_type = Ty::mk_procedure(
            self.tcx,
            Some(Procedure::SameModule {
                name: func_identifier_id,
            }),
            param_types,
            return_type,
        );

        env.bind(func_name, func_identifier_id, func_type);

        Ok(())
    }

    fn register_constant_signature(
        &mut self,
        env: &mut Environment<'cx, '_>,
        const_def: &ast::ConstantDef<'cx>,
    ) -> Result<()> {
        self.bind(
            env,
            const_def.name,
            self.resolve_type_from_path(const_def.ty)?,
        );

        Ok(())
    }
}
