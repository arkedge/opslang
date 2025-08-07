use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;
use std::sync::atomic::AtomicU32;
use typed_arena::Arena;

/// Represents the different kinds of types in the type system.
///
/// This enum defines all possible type variants that can exist in the language,
/// including primitive types, compound types, and type variables for inference.
#[derive(Debug, Clone, PartialEq)]
pub enum TyKind<'cx> {
    /// 32-bit signed integer type
    Int,
    /// 64-bit floating point number type
    Float,
    /// String type for text data
    String,
    /// Boolean type for true/false values
    Bool,
    /// Duration type for time intervals
    Duration,
    /// Time type for specific points in time
    Time,
    /// Array type containing elements of a specific inner type
    Array { inner: Ty<'cx> },
    /// Function type with argument types and return type
    Function { arg: Vec<Ty<'cx>>, ret: Ty<'cx> },
    /// Type variable used during type inference
    Variable(TypeVariable),
    /// Unit type representing no meaningful value
    Unit,
}

/// A type reference that points to a type kind.
///
/// This is a lightweight wrapper around a reference to TyKind that allows
/// for efficient sharing of type information across the type checker.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Ty<'cx>(pub &'cx TyKind<'cx>);

impl<'cx> Ty<'cx> {
    /// Returns a reference to the underlying type kind.
    ///
    /// This provides access to the actual type information stored within the type reference.
    pub fn kind(&self) -> &'cx TyKind<'cx> {
        self.0
    }
}

impl<'cx> std::ops::Deref for Ty<'cx> {
    type Target = &'cx TyKind<'cx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Represents a type variable used during type inference.
///
/// Type variables are placeholders for unknown types that get unified
/// during the type checking process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVariable(u32);

impl std::fmt::Display for TypeVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "$t{}", self.0)
    }
}

impl TypeVariable {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel))
    }
}

/// Represents an identifier with scope information.
///
/// Identifiers are used to distinguish variables and functions across different scopes,
/// allowing proper name resolution in nested contexts.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier<'cx> {
    /// The string name of the identifier
    pub name: &'cx str,
    /// The nesting depth where this identifier was defined
    pub scope_depth: usize,
}

/// A reference to an identifier.
///
/// This is a lightweight wrapper around an Identifier reference that enables
/// efficient passing and comparison of identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident<'cx>(&'cx Identifier<'cx>);

impl<'cx> Borrow<str> for Ident<'cx> {
    fn borrow(&self) -> &str {
        self.name
    }
}

impl<'cx> std::ops::Deref for Ident<'cx> {
    type Target = &'cx Identifier<'cx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cx> TyKind<'cx> {
    /// Performs the occurs check for the given type variable.
    ///
    /// This check prevents infinite types by ensuring a type variable doesn't
    /// occur within its own definition during unification.
    pub fn occurs(&self, var: TypeVariable) -> bool {
        match self {
            // Direct variable occurrence
            TyKind::Variable(v) => *v == var,
            // Recursively check array element type
            TyKind::Array { inner } => inner.occurs(var),
            // Check all argument types and return type
            TyKind::Function { arg: args, ret } => {
                args.iter().any(|arg| arg.occurs(var)) || ret.occurs(var)
            }
            // Primitive types cannot contain variables
            _ => false,
        }
    }

    /// Returns a human-readable string representation of the type.
    ///
    /// This method formats types in a user-friendly way for error messages
    /// and debugging output.
    pub fn display(&self, _cx: &TypingContext<'cx>) -> String {
        match self {
            TyKind::Int => "i32".to_string(),
            TyKind::Float => "f64".to_string(),
            TyKind::String => "string".to_string(),
            TyKind::Bool => "bool".to_string(),
            TyKind::Duration => "duration".to_string(),
            TyKind::Time => "time".to_string(),
            // Format array types as [element_type]
            TyKind::Array { inner } => format!("[{}]", inner.display(_cx)),
            // Format function types as (arg1, arg2, ...) -> return_type
            TyKind::Function { arg: args, ret } => {
                let arg_strs: Vec<String> = args.iter().map(|arg| arg.display(_cx)).collect();
                format!("({}) -> {}", arg_strs.join(", "), ret.display(_cx))
            }
            // Display type variables with a distinctive prefix
            TyKind::Variable(var) => format!("{var}"),
            TyKind::Unit => "()".to_string(),
        }
    }
}

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
    module_arena: Arena<Module<'cx>>,
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
            scope_depth: 0,
        }))
    }

    /// Allocates a module in the module arena and returns a reference.
    ///
    /// This allows modules to be stored with the same lifetime as the typing context,
    /// enabling safe references across the type checking process.
    pub fn alloc_module(&'cx self, module: Module<'cx>) -> &'cx Module<'cx> {
        self.module_arena.alloc(module)
    }

    /// Returns a displayable string representation of a type.
    ///
    /// This is a convenience method that delegates to the type's display method.
    pub fn display_type(&self, ty: Ty<'cx>) -> String {
        ty.display(self)
    }
}

impl<'cx> Default for TypingContext<'cx> {
    fn default() -> Self {
        Self::new()
    }
}

/// Represents different kinds of items that can exist within a module.
///
/// Module items define the public interface of a module, including constants,
/// type definitions, and function definitions that can be imported by other modules.
#[derive(Debug, Clone)]
pub enum ModuleItem<'cx> {
    /// A constant value with its associated type
    Constant { id: Ident<'cx>, ty: Ty<'cx> },
    /// A type definition with its concrete type
    Type { id: Ident<'cx>, ty: Ty<'cx> },
    /// A function definition with its function type
    Function { id: Ident<'cx>, ty: Ty<'cx> },
}

impl<'cx> ModuleItem<'cx> {
    /// Returns the identifier of this module item.
    ///
    /// This provides a uniform way to access the name regardless of the item type.
    pub fn id(&self) -> Ident<'cx> {
        match self {
            ModuleItem::Constant { id, .. } => *id,
            ModuleItem::Type { id, .. } => *id,
            ModuleItem::Function { id, .. } => *id,
        }
    }

    /// Returns the type associated with this module item.
    ///
    /// This provides a uniform way to access the type regardless of the item type.
    pub fn ty(&self) -> Ty<'cx> {
        match self {
            ModuleItem::Constant { ty, .. } => *ty,
            ModuleItem::Type { ty, .. } => *ty,
            ModuleItem::Function { ty, .. } => *ty,
        }
    }
}

/// Represents a module containing named items.
///
/// Modules provide namespacing and organization for types, functions, and constants.
/// Each module maintains a mapping from names to their corresponding items.
#[derive(Debug, Clone)]
pub struct Module<'cx> {
    /// The name of this module
    id: Ident<'cx>,
    /// Map from item names to their definitions
    items: HashMap<Ident<'cx>, ModuleItem<'cx>>,
}

impl<'cx> Module<'cx> {
    /// Creates a new empty module with the given name.
    ///
    /// The module starts with no items and can be populated using add_item.
    pub fn new(id: Ident<'cx>) -> Self {
        Self {
            id,
            items: HashMap::new(),
        }
    }

    /// Returns the name of this module.
    ///
    /// This provides read-only access to the module's name.
    pub fn name(&self) -> &str {
        self.id.name
    }

    /// Adds an item to this module.
    ///
    /// The item is indexed by its name, allowing for efficient lookup.
    /// If an item with the same name already exists, it will be replaced.
    pub fn add_item(&mut self, item: ModuleItem<'cx>) {
        self.items.insert(item.id(), item);
    }

    /// Looks up an item by name within this module.
    ///
    /// Returns None if no item with the given name exists in this module.
    pub fn lookup_item(&self, name: &str) -> Option<&ModuleItem<'cx>> {
        self.items.get(name)
    }

    /// Returns an iterator over all items in this module.
    ///
    /// This allows iteration over module contents without exposing the internal HashMap.
    pub fn items(&self) -> impl Iterator<Item = &ModuleItem<'cx>> {
        self.items.values()
    }
}

/// Manages loading and resolution of modules and their items.
///
/// The module loader maintains a registry of available modules and provides
/// path resolution functionality for finding items across modules.
#[derive(Debug)]
pub struct ModuleLoader<'cx> {
    /// Map from module names to their definitions
    modules: HashMap<Ident<'cx>, &'cx Module<'cx>>,
}

impl<'cx> ModuleLoader<'cx> {
    /// Creates a new empty module loader.
    ///
    /// The loader starts with no registered modules.
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }

    /// Registers a module with the loader under the given name.
    ///
    /// This makes the module available for path resolution and import operations.
    pub fn add_module(&mut self, module: &'cx Module<'cx>) {
        self.modules.insert(module.id, module);
    }

    /// Looks up a module by name.
    ///
    /// Returns None if no module with the given name is registered.
    pub fn lookup_module(&self, name: &str) -> Option<&'cx Module<'cx>> {
        self.modules.get(name).copied()
    }

    /// Resolves a path to a module item.
    ///
    /// Currently supports only flat paths that resolve to items in the builtin module.
    /// In the future, this will support hierarchical paths like "module::item".
    pub fn resolve_path(&self, path: &str) -> Option<ModuleItem<'cx>> {
        // Currently only supports non-hierarchical paths
        // Future enhancement: support "module::item" format
        if let Some(module) = self.modules.get("builtin") {
            module.lookup_item(path).cloned()
        } else {
            None
        }
    }
}

impl<'cx> Default for ModuleLoader<'cx> {
    fn default() -> Self {
        Self::new()
    }
}
