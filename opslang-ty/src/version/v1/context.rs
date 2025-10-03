pub use super::*;

/// The main context for type checking operations.
///
/// This structure manages memory allocation for types, identifiers, and modules
/// using arenas to ensure efficient memory usage and proper lifetime management.
pub struct TypingContext<'cx> {
    /// Arena for allocating type kinds
    type_arena: Arena<TyKind<'cx>>,
    /// Arena for allocating identifier information
    identifier_arena: Arena<Identifier<'cx>>,
    /// Arena for allocating module definitions
    module_arena: Arena<ModuleDef<'cx>>,
    /// Arena for allocating module items
    module_item_arena: Arena<ModuleItemDef<'cx>>,
    /// Global counter for unique definition IDs
    next_definition_id: AtomicUsize,
}

impl<'cx> TypingContext<'cx> {
    /// Creates a new typing context with empty arenas.
    ///
    /// This initializes all the necessary memory arenas for type checking operations.
    pub fn new() -> Self {
        Self {
            type_arena: Arena::new(),
            identifier_arena: Arena::new(),
            module_arena: Arena::new(),
            module_item_arena: Arena::new(),
            next_definition_id: AtomicUsize::new(1),
        }
    }

    /// Allocates a type in the type arena and returns a reference.
    ///
    /// This ensures that types have the same lifetime as the typing context
    /// and can be safely shared throughout the type checking process.
    pub fn alloc_type(&'cx self, ty: TyKind<'cx>) -> Ty<'cx> {
        Ty(self.type_arena.alloc(ty))
    }

    /// Allocates an identifier in the identifier arena and returns a reference.
    ///
    /// This provides a consistent way to manage identifier lifetimes and enables
    /// efficient comparison and storage of identifiers.
    pub fn alloc_ident(&'cx self, identifier: Identifier<'cx>) -> Ident<'cx> {
        Ident(self.identifier_arena.alloc(identifier))
    }

    /// Allocates a toplevel identifier in the identifier arena and returns a reference.
    ///
    /// This provides a consistent way to manage identifier lifetimes and enables
    /// efficient comparison and storage of identifiers.
    pub fn alloc_toplevel_ident(&'cx self, str: &'cx str) -> Ident<'cx> {
        Ident(self.identifier_arena.alloc(Identifier {
            name: str,
            definition_id: 0,
        }))
    }

    /// Allocates a new identifier with a unique definition ID.
    ///
    /// Each call to this method generates a globally unique identifier,
    /// enabling proper shadowing where multiple variables can have the same name.
    pub fn alloc_identifier(&'cx self, name: ast::Ident<'cx>) -> Ident<'cx> {
        let definition_id = self.next_definition_id.fetch_add(1, Ordering::SeqCst);
        Ident(self.identifier_arena.alloc(Identifier {
            name: name.raw,
            definition_id,
        }))
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

impl<'cx> Default for TypingContext<'cx> {
    fn default() -> Self {
        Self::new()
    }
}
