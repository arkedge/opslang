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
}

impl<'cx> Default for TypingContext<'cx> {
    fn default() -> Self {
        Self::new()
    }
}
