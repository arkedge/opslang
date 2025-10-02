use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn resolve_type_from_path(&self, path: ast::Path<'cx>) -> Result<Ty<'cx>> {
        match self.module_loader.resolve_path(path) {
            Some(item) => match item {
                opslang_ty::version::v1::ModuleItem::Type { ty, .. } => Ok(*ty),
                _ => Err(anyhow!("path `{path}` does not refer to a type")),
            },
            None => Err(anyhow!("unknown type: {path}")),
        }
    }

    pub(super) fn resolve_path(&self, path: &'cx ast::Path<'cx>) -> Result<ResolvePathResult<'cx>> {
        match self.module_loader.resolve_path(*path) {
            Some(item) => {
                let resolved_path = ir::ResolvedPath {
                    item: opslang_ir::version::ResolvedItem::ModuleItem(item),
                    original_path: path,
                };
                Ok(ResolvePathResult {
                    resolved_path,
                    item,
                })
            }
            None => Err(anyhow!("cannot resolve path: {path}")),
        }
    }

    /// Eagerly resolves type variables in the given type using the provided substitution.
    pub(super) fn eagerly_resolve(
        &self,
        subst: &mut Substitution<'cx>,
        ty: &mut Ty<'cx>,
    ) -> Result<()> {
        // call `visit_ty_mut` to resolve type variables eagerly
        hm::SubstitutionVisitor::new_borrowed(subst, self.tcx).visit_mut(ty);
        self.require_resolved(*ty)
    }

    pub(super) fn require_resolved(&self, ty: Ty<'cx>) -> Result<()> {
        if ty.is_infer() {
            Err(anyhow!("type `{ty}` must be resolved at this point"))
        } else {
            Ok(())
        }
    }

    pub(super) fn try_external_resolve(&self, path: ast::Path<'cx>) -> Option<Ty<'cx>> {
        self.external_resolver.as_ref()?.resolve(path, self.tcx)
    }
}
