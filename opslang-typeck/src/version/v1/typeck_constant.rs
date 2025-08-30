use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn typeck_constant(
        &mut self,
        env: &Environment<'cx, '_>,
        const_def: &'cx ast::ConstantDef<'cx>,
    ) -> Result<ast::ConstantDef<'cx, IrTypeFamily>> {
        let declared_type = self.resolve_type_from_path(const_def.ty.raw)?;
        let mut subst = Substitution::new();
        let mut inferred_expr = self.typeck_expr(env, &mut subst, &const_def.value)?;

        self.unify(&mut subst, declared_type, inferred_expr.ty)?;

        // Apply final substitution using visitor
        let mut visitor = SubstitutionVisitor::new(subst, self.typing_cx);
        visitor.visit_mut(&mut inferred_expr);

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
}
