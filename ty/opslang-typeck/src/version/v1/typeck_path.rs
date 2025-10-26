use super::*;

impl<'cx> TypeChecker<'cx> {
    /// Type checks a path expression, resolving it to a variable or module item.
    pub(super) fn typeck_path<'env>(
        &mut self,
        env: &Scope<'cx, 'env>,
        path: &'cx ast::Path<'cx>,
    ) -> Result<(ir::ResolvedPath<'cx>, Ty<'cx>)> {
        // First check if this is a single identifier that can be resolved in local environment
        if let Some(ident) = path.is_ident()
            && let Some(environment::TypedIdent { id, ty }) = env.lookup_var(ident)
        {
            let resolved_path = ir::ResolvedPath {
                item: ir::ResolvedItem::LocalVariable(id),
                original_path: path,
            };

            return Ok((resolved_path, ty));
        }

        // Fall back to module resolution
        if let Ok(ResolvePathResult {
            resolved_path,
            item,
        }) = self.resolve_path(path)
        {
            match &*item {
                ModuleItemDef::Constant { ty, .. } | ModuleItemDef::Prc { ty, .. } => {
                    return Ok((resolved_path, *ty));
                }
                ModuleItemDef::LibraryFn { ty: poly_ty, .. } => {
                    // Instantiate polymorphic type with fresh type variables
                    let instantiated_ty = poly_ty.instantiate(self.tcx);
                    return Ok((resolved_path, instantiated_ty));
                }
                _ => {}
            }
        }

        // Try external resolver if available
        if let Some(ty) = self.try_external_resolve(*path) {
            let resolved_path = ir::ResolvedPath {
                item: ir::ResolvedItem::External,
                original_path: path,
            };
            let wrapped_ty = Ty::mk_external(self.tcx, *path, ty);
            return Ok((resolved_path, wrapped_ty));
        }

        // Not found
        Err(anyhow!("unbound variable: {path}"))
    }
}
