use opslang_visitor_macro::Visit;
use typed_arena::Arena;

mod module;
pub use module::{AlreadyDefinedError, Module, ModuleDef, ModuleItem, ModuleItemDef, ModuleLoader};

pub mod context;
pub use context::ModuleContext;
