use super::*;

/// The module context for type checking operations.
///
/// This structure manages memory allocation for types, identifiers, and modules
/// using arenas to ensure efficient memory usage and proper lifetime management.
pub struct ModuleContext<'cx> {
    /// Arena for allocating module paths.
    path_arena: Arena<ModulePathData<'cx>>,
    /// Arena for allocating module definitions.
    module_arena: Arena<ModuleDef<'cx>>,
    /// Arena for allocating module items.
    module_item_arena: Arena<ModuleItemDef<'cx>>,
}

impl<'cx> ModuleContext<'cx> {
    /// Creates a new typing context with empty arenas.
    ///
    /// This initializes all the necessary memory arenas for type checking operations.
    pub fn new() -> Self {
        Self {
            path_arena: Arena::new(),
            module_arena: Arena::new(),
            module_item_arena: Arena::new(),
        }
    }

    /// Creates a root-level module path.
    pub fn alloc_root_path(&'cx self, segment: &'cx str) -> ModulePath<'cx> {
        let data = ModulePathData {
            segment,
            parent: None,
        };
        ModulePath(self.path_arena.alloc(data))
    }

    /// Creates a child module path extending a parent.
    pub fn alloc_child_path(
        &'cx self,
        parent: ModulePath<'cx>,
        segment: &'cx str,
    ) -> ModulePath<'cx> {
        let data = ModulePathData {
            segment,
            parent: Some(parent),
        };
        ModulePath(self.path_arena.alloc(data))
    }

    /// Allocates a module in the module arena and returns a reference.
    ///
    /// This allows modules to be stored with the same lifetime as the typing context,
    /// enabling safe references across the type checking process.
    pub fn alloc_module(&'cx self, module: ModuleDef<'cx>) -> Module<'cx> {
        Module(self.module_arena.alloc(module))
    }

    /// Allocates a module item in the module item arena and returns a reference.
    ///
    /// This allows module items to be stored with the same lifetime as the typing context.
    pub fn alloc_module_item(&'cx self, item: ModuleItemDef<'cx>) -> ModuleItem<'cx> {
        ModuleItem(self.module_item_arena.alloc(item))
    }
}

impl<'cx> Default for ModuleContext<'cx> {
    fn default() -> Self {
        Self::new()
    }
}
