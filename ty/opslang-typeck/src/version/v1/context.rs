use opslang_module::version::v1::ModuleContext;
use opslang_ty::version::v1::TypingContext;

#[derive(Clone, Copy)]
pub struct GlobalContext<'cx> {
    pub tcx: &'cx TypingContext<'cx>,
    pub module: &'cx ModuleContext<'cx>,
}

impl<'cx> std::ops::Deref for GlobalContext<'cx> {
    type Target = &'cx TypingContext<'cx>;

    fn deref(&self) -> &Self::Target {
        &self.tcx
    }
}
