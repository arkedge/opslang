/*!
Type system for opslang v1.

This module defines the type system structures including type kinds, identifiers, and
module management for the opslang v1 implementation.

**IMPORTANT**: When modifying type structures in this module, update the visitor type
registry in `opslang-ir-macro/src/visitor_type_registry.rs` to ensure proper visitor
macro generation. Choose whether to make types hookable (add to node types) or not
hookable (add to inter types).
*/

use opslang_visitor_macro::Visit;
use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};
use typed_arena::Arena;

use opslang_ast::syntax::v1 as ast;

pub mod hm;
pub use hm::{IntVarValue, Substitution};

/// Signed integer types, following Rust's naming convention.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
}

/// Unsigned integer types, following Rust's naming convention.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
}

/// Floating point types, following Rust's naming convention.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub enum FloatTy {
    F32,
    F64,
}

/// Represents the different kinds of types in the type system.
///
/// This enum defines all possible type variants that can exist in the language,
/// including primitive types, compound types, and type variables for inference.
#[derive(Debug, Clone, PartialEq, Visit)]
pub enum TyKind<'cx> {
    /// Signed integer types.
    Int(IntTy),

    /// Unsigned integer types.
    Uint(UintTy),

    /// Floating point types.
    Float(FloatTy),

    /// String type for text data.
    String,

    /// Byte array type for binary data.
    Bytes,

    /// Boolean type for true/false values.
    Bool,

    /// Duration type for time intervals.
    Duration,

    /// Time type for specific points in time.
    Time,

    /// Array type containing elements of a specific inner type.
    Array { inner: Ty<'cx> },

    /// Function type with argument types and return type.
    Function {
        arg: Vec<Ty<'cx>>,
        ret: Ty<'cx>,

        /// Whether this is a procedure.
        is_procedure: Option<Procedure<'cx>>,
    },

    /// Inference variable used during type inference.
    Infer(InferTy),

    /// Unit type representing no meaningful value.
    Unit,

    /// External type identified by a string name.
    External { path: ast::Path<'cx>, ty: Ty<'cx> },
}

#[derive(Debug, Clone, Copy, PartialEq, Visit)]
pub enum Procedure<'cx> {
    SameModule { name: Ident<'cx> },
    // FIXME: Add cross-module procedures
    External,
}

/// A type reference that points to a type kind.
///
/// This is a lightweight wrapper around a reference to TyKind that allows
/// for efficient sharing of type information across the type checker.
#[derive(Debug, Clone, Copy, PartialEq, Visit)]
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

impl Display for Ty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// Integer type variable for type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub struct IntVid(u32);

impl IntVid {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel))
    }
}

impl std::fmt::Display for IntVid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "$int{}", self.0)
    }
}

/// Float type variable for type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub struct FloatVid(u32);

impl FloatVid {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel))
    }
}

impl std::fmt::Display for FloatVid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "$float{}", self.0)
    }
}

/// General type variable for type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub struct TyVid(u32);

impl TyVid {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel))
    }
}

impl std::fmt::Display for TyVid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "$t{}", self.0)
    }
}

/// Represents inference variables used during type inference.
///
/// Different kinds of inference variables allow for more precise type inference,
/// particularly for numeric types that can have default fallbacks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
pub enum InferTy {
    /// General type inference variable
    TyVar(TyVid),
    /// Integer inference variable that can fallback to default integer type
    IntVar(IntVid),
    /// Float inference variable that can fallback to default float type  
    FloatVar(FloatVid),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Integer {
    Int(IntTy),
    Uint(UintTy),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Numeric {
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
}

/// Represents an identifier with scope information.
///
/// Identifiers are used to distinguish variables and functions across different scopes,
/// allowing proper name resolution in nested contexts.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub struct Identifier<'cx> {
    /// The string name of the identifier
    pub name: &'cx str,
    /// The unique definition ID for this identifier
    pub definition_id: usize,
}

/// A reference to an identifier.
///
/// This is a lightweight wrapper around an Identifier reference that enables
/// efficient passing and comparison of identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
pub struct Ident<'cx>(pub &'cx Identifier<'cx>);

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
    pub fn occurs(&self, var: TyVid) -> bool {
        match self {
            // Direct variable occurrence
            TyKind::Infer(InferTy::TyVar(v)) => *v == var,
            // IntVar and FloatVar cannot occur in general type variables
            TyKind::Infer(InferTy::IntVar(_) | InferTy::FloatVar(_)) => false,
            // Recursively check array element type
            TyKind::Array { inner } => inner.occurs(var),
            // Check all argument types and return type
            TyKind::Function {
                arg,
                ret,
                is_procedure: _,
            } => arg.iter().any(|arg| arg.occurs(var)) || ret.occurs(var),
            // Primitive types cannot contain variables
            _ => false,
        }
    }

    /// Returns `true` if the ty kind is [`Bool`].
    ///
    /// [`Bool`]: TyKind::Bool
    #[must_use]
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    /// Returns `true` if the ty kind is [`Duration`].
    ///
    /// [`Duration`]: TyKind::Duration
    #[must_use]
    pub fn is_duration(&self) -> bool {
        matches!(self, Self::Duration)
    }

    /// Returns `true` if the ty kind is [`Time`].
    ///
    /// [`Time`]: TyKind::Time
    #[must_use]
    pub fn is_time(&self) -> bool {
        matches!(self, Self::Time)
    }

    /// Returns `true` if the ty kind is [`Unit`].
    ///
    /// [`Unit`]: TyKind::Unit
    #[must_use]
    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    #[must_use]
    pub fn as_numeric(&self) -> Option<Numeric> {
        match self {
            TyKind::Int(int_ty) => Some(Numeric::Int(*int_ty)),
            TyKind::Uint(uint_ty) => Some(Numeric::Uint(*uint_ty)),
            TyKind::Float(float_ty) => Some(Numeric::Float(*float_ty)),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_numeric(&self) -> bool {
        matches!(self, TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_))
    }

    #[must_use]
    pub fn as_integer(&self) -> Option<Integer> {
        match self {
            TyKind::Int(int_ty) => Some(Integer::Int(*int_ty)),
            TyKind::Uint(uint_ty) => Some(Integer::Uint(*uint_ty)),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_integer(&self) -> bool {
        matches!(self, TyKind::Int(_) | TyKind::Uint(_))
    }

    /// Returns `true` if the ty kind is [`Infer`].
    ///
    /// [`Infer`]: TyKind::Infer
    #[must_use]
    pub fn is_infer(&self) -> bool {
        matches!(self, Self::Infer(..))
    }
}

/// Represents a polymorphic type with type variables.
///
/// A polymorphic type consists of type variables (TyVar) that can be instantiated
/// to create monomorphic types. This enables generic functions and types.
#[derive(Debug, Clone, PartialEq, Visit)]
pub struct PolyTy<'cx> {
    /// The type variables bound by this polymorphic type.
    pub type_vars: Vec<TyVid>,
    /// The body type that may contain the bound type variables.
    pub body: Ty<'cx>,
}

impl<'cx> PolyTy<'cx> {
    /// Creates a new polymorphic type.
    pub fn new(type_vars: Vec<TyVid>, body: Ty<'cx>) -> Self {
        Self { type_vars, body }
    }

    /// Creates a monomorphic type (no type variables).
    pub fn mono(body: Ty<'cx>) -> Self {
        Self {
            type_vars: Vec::new(),
            body,
        }
    }

    /// Instantiates the polymorphic type with fresh type variables.
    ///
    /// This creates a new monomorphic type by replacing bound type variables
    /// with fresh type variables for inference.
    pub fn instantiate(&self, cx: &'cx TypingContext<'cx>) -> Ty<'cx> {
        if self.type_vars.is_empty() {
            return self.body;
        }

        let mut substitution = hm::Substitution::new();
        for &var in &self.type_vars {
            let fresh_var = TyVid::fresh();
            let fresh_ty = Ty::mk_variable(cx, fresh_var);
            substitution.insert(var, fresh_ty);
        }
        substitution.apply_substitution_pure(cx, self.body)
    }
}

impl<'cx> Ty<'cx> {
    pub fn from_kind(cx: &'cx TypingContext<'cx>, kind: TyKind<'cx>) -> Self {
        cx.alloc_type(kind)
    }

    pub fn mk_int(cx: &'cx TypingContext<'cx>, int_ty: IntTy) -> Self {
        Self::from_kind(cx, TyKind::Int(int_ty))
    }

    pub fn mk_uint(cx: &'cx TypingContext<'cx>, uint_ty: UintTy) -> Self {
        Self::from_kind(cx, TyKind::Uint(uint_ty))
    }

    pub fn mk_float(cx: &'cx TypingContext<'cx>, float_ty: FloatTy) -> Self {
        Self::from_kind(cx, TyKind::Float(float_ty))
    }

    pub fn mk_i8(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_int(cx, IntTy::I8)
    }

    pub fn mk_i16(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_int(cx, IntTy::I16)
    }

    pub fn mk_i32(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_int(cx, IntTy::I32)
    }

    pub fn mk_i64(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_int(cx, IntTy::I64)
    }

    pub fn mk_u8(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_uint(cx, UintTy::U8)
    }

    pub fn mk_u16(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_uint(cx, UintTy::U16)
    }

    pub fn mk_u32(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_uint(cx, UintTy::U32)
    }

    pub fn mk_u64(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_uint(cx, UintTy::U64)
    }

    pub fn mk_f32(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_float(cx, FloatTy::F32)
    }

    pub fn mk_f64(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_float(cx, FloatTy::F64)
    }

    pub fn mk_string(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::String)
    }

    pub fn mk_bytes(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Bytes)
    }

    pub fn mk_bool(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Bool)
    }

    pub fn mk_duration(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Duration)
    }

    pub fn mk_time(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Time)
    }

    pub fn mk_array(cx: &'cx TypingContext<'cx>, inner: Ty<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Array { inner })
    }

    pub fn mk_procedure(
        cx: &'cx TypingContext<'cx>,
        procedure: Option<Procedure<'cx>>,
        arg: Vec<Ty<'cx>>,
        ret: Ty<'cx>,
    ) -> Self {
        Self::from_kind(
            cx,
            TyKind::Function {
                arg,
                ret,
                is_procedure: procedure,
            },
        )
    }

    pub fn mk_function(cx: &'cx TypingContext<'cx>, arg: Vec<Ty<'cx>>, ret: Ty<'cx>) -> Self {
        Self::from_kind(
            cx,
            TyKind::Function {
                arg,
                ret,
                is_procedure: None,
            },
        )
    }

    pub fn mk_variable(cx: &'cx TypingContext<'cx>, var: TyVid) -> Self {
        Self::from_kind(cx, TyKind::Infer(InferTy::TyVar(var)))
    }

    pub fn mk_fresh(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_variable(cx, TyVid::fresh())
    }

    pub fn mk_int_var(cx: &'cx TypingContext<'cx>, var: IntVid) -> Self {
        Self::from_kind(cx, TyKind::Infer(InferTy::IntVar(var)))
    }

    pub fn mk_float_var(cx: &'cx TypingContext<'cx>, var: FloatVid) -> Self {
        Self::from_kind(cx, TyKind::Infer(InferTy::FloatVar(var)))
    }

    pub fn mk_unit(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Unit)
    }

    pub fn mk_external(cx: &'cx TypingContext<'cx>, path: ast::Path<'cx>, ty: Ty<'cx>) -> Self {
        Self::from_kind(cx, TyKind::External { path, ty })
    }
}

impl Display for TyKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let e = match self {
            TyKind::Int(int_ty) => match int_ty {
                IntTy::I8 => "i8",
                IntTy::I16 => "i16",
                IntTy::I32 => "i32",
                IntTy::I64 => "i64",
            },
            TyKind::Uint(uint_ty) => match uint_ty {
                UintTy::U8 => "u8",
                UintTy::U16 => "u16",
                UintTy::U32 => "u32",
                UintTy::U64 => "u64",
            },
            TyKind::Float(float_ty) => match float_ty {
                FloatTy::F32 => "f32",
                FloatTy::F64 => "f64",
            },
            TyKind::String => "string",
            TyKind::Bytes => "bytes",
            TyKind::Bool => "bool",
            TyKind::Duration => "duration",
            TyKind::Time => "time",
            TyKind::Unit => "()",
            // Format array types as [element_type]
            TyKind::Array { inner } => return write!(f, "[{inner}]"),
            // Format function types as (arg1, arg2, ...) -> return_type
            TyKind::Function {
                arg,
                ret,
                is_procedure,
            } => {
                let arg_strs: Vec<String> = arg.iter().map(|arg| arg.to_string()).collect();
                return if is_procedure.is_some() {
                    write!(f, "prc ({}) -> {ret}", arg_strs.join(", "),)
                } else {
                    write!(f, "({}) -> {ret}", arg_strs.join(", "),)
                };
            }
            // Display inference variables with distinctive prefixes
            TyKind::Infer(infer_ty) => match infer_ty {
                InferTy::TyVar(var) => return write!(f, "{var}"),
                InferTy::IntVar(var) => return write!(f, "{var}"),
                InferTy::FloatVar(var) => return write!(f, "{var}"),
            },
            TyKind::External { path, ty } => return write!(f, "(extern<`{path}`>: {ty})"),
        };
        write!(f, "{e}")
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
    /// Arena for allocating module items
    module_item_arena: Arena<ModuleItem<'cx>>,
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
    pub fn alloc_identifier(&'cx self, name: &'cx str) -> Ident<'cx> {
        let definition_id = self.next_definition_id.fetch_add(1, Ordering::SeqCst);
        Ident(self.identifier_arena.alloc(Identifier {
            name,
            definition_id,
        }))
    }

    /// Allocates a module in the module arena and returns a reference.
    ///
    /// This allows modules to be stored with the same lifetime as the typing context,
    /// enabling safe references across the type checking process.
    pub fn alloc_module(&'cx self, module: Module<'cx>) -> &'cx Module<'cx> {
        self.module_arena.alloc(module)
    }

    /// Allocates a module item in the module item arena and returns a reference.
    ///
    /// This allows module items to be stored with the same lifetime as the typing context.
    pub fn alloc_module_item(&'cx self, item: ModuleItem<'cx>) -> &'cx ModuleItem<'cx> {
        self.module_item_arena.alloc(item)
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
#[derive(Debug, Clone, Visit, PartialEq)]
#[skip_all_visit]
pub enum ModuleItem<'cx> {
    /// A constant value.
    Constant {
        /// The identifier of this module item
        id: Ident<'cx>,
        /// The type associated with this module item
        ty: Ty<'cx>,
    },
    /// A type definition.
    Type {
        /// The identifier of this module item
        id: Ident<'cx>,
        /// The type associated with this module item
        ty: Ty<'cx>,
    },
    /// A procedure definition.
    Prc {
        /// The identifier of this module item
        id: Ident<'cx>,
        /// The type associated with this module item
        ty: Ty<'cx>,
    },
    /// A library function definition with polymorphic type.
    LibraryFn {
        /// The identifier of this module item
        id: Ident<'cx>,
        /// The polymorphic type associated with this module item
        ty: PolyTy<'cx>,
    },
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
    items: HashMap<String, ModuleItem<'cx>>,
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
    fn add_item(&mut self, item: ModuleItem<'cx>) {
        let id = match &item {
            ModuleItem::Constant { id, .. } => id,
            ModuleItem::Type { id, .. } => id,
            ModuleItem::Prc { id, .. } => id,
            ModuleItem::LibraryFn { id, .. } => id,
        };
        self.items.insert(id.name.to_string(), item);
    }

    /// Creates a new constant module item.
    pub fn add_constant(&mut self, id: Ident<'cx>, ty: Ty<'cx>) {
        self.add_item(ModuleItem::Constant { id, ty })
    }

    /// Creates a new type module item.
    pub fn add_type(&mut self, id: Ident<'cx>, ty: Ty<'cx>) {
        self.add_item(ModuleItem::Type { id, ty })
    }

    /// Creates a new procedure module item.
    pub fn add_prc(&mut self, id: Ident<'cx>, ty: Ty<'cx>) {
        self.add_item(ModuleItem::Prc { id, ty })
    }

    /// Creates a new library function module item, which can be polymorphic.
    pub fn add_library_function(&mut self, id: Ident<'cx>, ty: PolyTy<'cx>) {
        self.add_item(ModuleItem::LibraryFn { id, ty })
    }

    /// Looks up an item by name within this module.
    ///
    /// Returns None if no item with the given name exists in this module.
    pub fn lookup_item(&self, name: opslang_ast::Path<'cx>) -> Option<&ModuleItem<'cx>> {
        self.items.get(&name.to_string())
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
    modules: HashMap<String, &'cx Module<'cx>>,
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
        self.modules.insert(module.id.name.to_string(), module);
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
    pub fn resolve_path(&self, path: opslang_ast::Path<'cx>) -> Option<&'cx ModuleItem<'cx>> {
        // Currently only supports non-hierarchical paths
        // Future enhancement: support "module::item" format
        if let Some(module) = self.modules.get("builtin") {
            module.lookup_item(path)
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
