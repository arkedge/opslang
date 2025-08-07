use anyhow::anyhow;
use opslang_ast::v1::{self as ast, DefaultTypeFamily, ExprKind, StatementKind};
use std::collections::HashMap;
use std::hash::Hash;
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
            TyKind::Variable(var) => format!("$t{}", var.0),
            TyKind::Unit => "()".to_string(),
        }
    }
}

type Result<T, E = anyhow::Error> = std::result::Result<T, E>;

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
    pub fn alloc_identifier(&'cx self, identifier: Identifier<'cx>) -> Ident<'cx> {
        Ident(self.identifier_arena.alloc(identifier))
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
    Constant { name: String, ty: Ty<'cx> },
    /// A type definition with its concrete type
    Type { name: String, ty: Ty<'cx> },
    /// A function definition with its function type
    Function { name: String, ty: Ty<'cx> },
}

impl<'cx> ModuleItem<'cx> {
    /// Returns the name of this module item.
    ///
    /// This provides a uniform way to access the name regardless of the item type.
    pub fn name(&self) -> &str {
        match self {
            ModuleItem::Constant { name, .. } => name,
            ModuleItem::Type { name, .. } => name,
            ModuleItem::Function { name, .. } => name,
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
    name: String,
    /// Map from item names to their definitions
    items: HashMap<String, ModuleItem<'cx>>,
}

impl<'cx> Module<'cx> {
    /// Creates a new empty module with the given name.
    ///
    /// The module starts with no items and can be populated using add_item.
    pub fn new(name: String) -> Self {
        Self {
            name,
            items: HashMap::new(),
        }
    }

    /// Returns the name of this module.
    ///
    /// This provides read-only access to the module's name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Adds an item to this module.
    ///
    /// The item is indexed by its name, allowing for efficient lookup.
    /// If an item with the same name already exists, it will be replaced.
    pub fn add_item(&mut self, item: ModuleItem<'cx>) {
        self.items.insert(item.name().to_string(), item);
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
    pub fn add_module(&mut self, name: String, module: &'cx Module<'cx>) {
        self.modules.insert(name, module);
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

/// Creates the builtin module containing all primitive types.
///
/// The builtin module provides access to fundamental types like integers, strings,
/// and other primitives that are available in all contexts without explicit imports.
pub fn create_builtin_module<'cx>(cx: &'cx TypingContext<'cx>) -> &'cx Module<'cx> {
    let mut builtin = Module::new("builtin".to_string());

    // Add all builtin primitive types
    builtin.add_item(ModuleItem::Type {
        name: "i32".to_string(),
        ty: cx.alloc_type(TyKind::Int),
    });
    builtin.add_item(ModuleItem::Type {
        name: "f64".to_string(),
        ty: cx.alloc_type(TyKind::Float),
    });
    builtin.add_item(ModuleItem::Type {
        name: "string".to_string(),
        ty: cx.alloc_type(TyKind::String),
    });
    builtin.add_item(ModuleItem::Type {
        name: "bool".to_string(),
        ty: cx.alloc_type(TyKind::Bool),
    });
    builtin.add_item(ModuleItem::Type {
        name: "duration".to_string(),
        ty: cx.alloc_type(TyKind::Duration),
    });
    builtin.add_item(ModuleItem::Type {
        name: "time".to_string(),
        ty: cx.alloc_type(TyKind::Time),
    });
    builtin.add_item(ModuleItem::Type {
        name: "()".to_string(),
        ty: cx.alloc_type(TyKind::Unit),
    });

    cx.alloc_module(builtin)
}

/// Represents a type substitution mapping type variables to concrete types.
///
/// Substitutions are the result of unification operations and are used to
/// replace type variables with their inferred concrete types throughout the type system.
#[derive(Debug, Clone, Default)]
pub struct Substitution<'cx> {
    /// Maps type variables to their substituted types
    map: HashMap<TypeVariable, Ty<'cx>>,
}

impl<'cx> Substitution<'cx> {
    /// Creates a new empty substitution.
    ///
    /// An empty substitution represents the identity mapping where no variables are substituted.
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Inserts a mapping from a type variable to a concrete type.
    ///
    /// This adds or replaces the substitution for the given type variable.
    pub fn insert(&mut self, var: TypeVariable, ty: Ty<'cx>) {
        self.map.insert(var, ty);
    }

    /// Gets the substituted type for a given type variable.
    ///
    /// Returns None if no substitution exists for the variable.
    pub fn get(&self, var: &TypeVariable) -> Option<Ty<'cx>> {
        self.map.get(var).copied()
    }

    /// Checks if this substitution is empty (contains no mappings).
    ///
    /// An empty substitution is equivalent to the identity substitution.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Applies this substitution to a type, replacing type variables with their substituted types.
    ///
    /// This recursively walks through the type structure and applies substitutions to all
    /// type variables found. The process continues until no more substitutions can be applied.
    pub fn apply_substitution(&self, cx: &'cx TypingContext<'cx>, ty: Ty<'cx>) -> Ty<'cx> {
        match ty.kind() {
            TyKind::Variable(var) => {
                // Recursively apply substitutions to handle chains of substitutions
                if let Some(substituted) = self.map.get(var) {
                    self.apply_substitution(cx, *substituted)
                } else {
                    ty
                }
            }
            TyKind::Array { inner } => {
                let substituted_inner = self.apply_substitution(cx, *inner);
                // Only allocate a new type if something actually changed
                if substituted_inner == *inner {
                    ty
                } else {
                    cx.alloc_type(TyKind::Array {
                        inner: substituted_inner,
                    })
                }
            }
            TyKind::Function { arg: args, ret } => {
                let mut changed = false;
                // Apply substitutions to all argument types
                let substituted_args: Vec<Ty<'cx>> = args
                    .iter()
                    .map(|&arg| {
                        let substituted = self.apply_substitution(cx, arg);
                        if substituted != arg {
                            changed = true;
                        }
                        substituted
                    })
                    .collect();
                // Apply substitution to return type
                let substituted_ret = self.apply_substitution(cx, *ret);
                if substituted_ret != *ret {
                    changed = true;
                }

                // Only allocate new function type if something changed
                if changed {
                    cx.alloc_type(TyKind::Function {
                        arg: substituted_args,
                        ret: substituted_ret,
                    })
                } else {
                    ty
                }
            }
            // Primitive types don't contain variables, so return as-is
            _ => ty,
        }
    }

    /// Composes this substitution with another substitution.
    ///
    /// The composition applies the first substitution to the types in the second substitution,
    /// then combines both mappings. This ensures that variable chains are properly resolved.
    pub fn compose(
        &self,
        other: &Substitution<'cx>,
        cx: &'cx TypingContext<'cx>,
    ) -> Substitution<'cx> {
        let mut result = self.clone();

        // Apply this substitution to all types in the other substitution
        for (var, &ty) in &other.map {
            let substituted_ty = self.apply_substitution(cx, ty);
            result.insert(*var, substituted_ty);
        }

        result
    }
}

/// Represents a lexical environment for name and type bindings.
///
/// Environments form a chain through parent references, enabling proper
/// lexical scoping where inner scopes can shadow outer scope bindings.
#[derive(Debug)]
pub struct Environment<'cx, 'env> {
    /// Maps variable names to their unique identifiers
    name_bindings: HashMap<&'cx str, Ident<'cx>>,
    /// Maps identifiers to their types
    type_bindings: HashMap<Ident<'cx>, Ty<'cx>>,
    /// Reference to parent environment for scope chaining
    parent: Option<&'env Self>,
    /// Nesting depth of this scope
    scope_depth: usize,
}

impl<'cx, 'env> Environment<'cx, 'env> {
    /// Creates a new top-level environment with no parent.
    ///
    /// This represents the global scope and has depth 0.
    pub fn new() -> Self {
        Self {
            name_bindings: HashMap::new(),
            type_bindings: HashMap::new(),
            parent: None,
            scope_depth: 0,
        }
    }

    /// Creates a new environment that extends a parent environment.
    ///
    /// The new environment has one greater depth than its parent and can access
    /// bindings from the parent chain while allowing local shadowing.
    pub fn extend_inherit(&'env self) -> Self {
        let scope_depth = self.scope_depth + 1;
        Self {
            name_bindings: HashMap::new(),
            type_bindings: HashMap::new(),
            parent: Some(self),
            scope_depth,
        }
    }

    /// Binds a name to an identifier and associates the identifier with a type.
    ///
    /// This is a convenience method that performs both name and type binding in one operation.
    pub fn bind(&mut self, name: &'cx str, id: Ident<'cx>, ty: Ty<'cx>) {
        self.name_bindings.insert(name, id);
        self.type_bindings.insert(id, ty);
    }

    /// Binds a name to an identifier without associating a type.
    ///
    /// This is used when the type will be bound separately or is not yet known.
    pub fn bind_name(&mut self, name: &'cx str, id: Ident<'cx>) {
        self.name_bindings.insert(name, id);
    }

    /// Associates an identifier with a type.
    ///
    /// This is used when the identifier is already known but its type needs to be recorded.
    pub fn bind_type(&mut self, id: Ident<'cx>, ty: Ty<'cx>) {
        self.type_bindings.insert(id, ty);
    }

    /// Looks up a name to find its associated identifier.
    ///
    /// Searches the current environment first, then walks up the parent chain.
    /// Returns None if the name is not bound in any accessible scope.
    pub fn lookup_name(&self, name: &str) -> Option<Ident<'cx>> {
        self.name_bindings
            .get(name)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.lookup_name(name)))
    }

    /// Looks up the type associated with an identifier.
    ///
    /// Searches the current environment first, then walks up the parent chain.
    /// Returns None if the identifier is not associated with any type in accessible scopes.
    pub fn lookup_type(&self, id: Ident<'cx>) -> Option<Ty<'cx>> {
        self.type_bindings
            .get(&id)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.lookup_type(id)))
    }

    /// Looks up a variable by name and returns its type.
    ///
    /// This combines name lookup and type lookup into a single operation,
    /// which is the most common operation during type checking.
    pub fn lookup_variable(&self, name: &str) -> Option<Ty<'cx>> {
        if let Some(id) = self.lookup_name(name) {
            self.lookup_type(id)
        } else {
            None
        }
    }

    /// Returns the nesting depth of this environment.
    ///
    /// The global environment has depth 0, with each nested scope incrementing the depth.
    pub fn scope_depth(&self) -> usize {
        self.scope_depth
    }
}

impl<'cx, 'env> Default for Environment<'cx, 'env> {
    fn default() -> Self {
        Self::new()
    }
}

/// The main type checker that performs type inference and checking.
///
/// The type checker maintains state for generating fresh type variables
/// and manages module loading for resolving external types and functions.
#[derive(Debug)]
pub struct TypeChecker<'cx> {
    /// Counter for generating unique type variable IDs
    next_var: u32,
    /// Module loader for resolving external symbols
    module_loader: ModuleLoader<'cx>,
}

impl<'cx> TypeChecker<'cx> {
    /// Creates a new type checker with empty module loader.
    ///
    /// The type checker starts with no modules loaded and a fresh variable counter.
    pub fn new() -> Self {
        Self {
            next_var: 0,
            module_loader: ModuleLoader::new(),
        }
    }

    /// Creates a new type checker with the given module loader.
    ///
    /// This allows pre-loading modules before starting type checking operations.
    pub fn with_module_loader(module_loader: ModuleLoader<'cx>) -> Self {
        Self {
            next_var: 0,
            module_loader,
        }
    }

    /// Adds a module to the type checker's module loader.
    ///
    /// This makes the module's exported items available for type resolution.
    pub fn add_module(&mut self, name: String, module: &'cx Module<'cx>) {
        self.module_loader.add_module(name, module);
    }

    /// Generates a fresh type variable with a unique identifier.
    ///
    /// Fresh type variables are used during type inference to represent unknown types
    /// that will be unified with concrete types during the checking process.
    pub fn fresh_type_var(&mut self) -> TypeVariable {
        let var = TypeVariable(self.next_var);
        self.next_var += 1;
        var
    }

    /// Attempts to unify two types, producing a substitution that makes them equal.
    ///
    /// Unification is the core algorithm for type inference, determining what type variables
    /// must be bound to make two types compatible. This implements the standard unification
    /// algorithm with occurs check to prevent infinite types.
    pub fn unify<'a>(
        cx: &'a TypingContext<'a>,
        t1: Ty<'a>,
        t2: Ty<'a>,
    ) -> Result<Substitution<'a>> {
        match (t1.kind(), t2.kind()) {
            // Two identical type variables unify trivially
            (TyKind::Variable(var1), TyKind::Variable(var2)) if var1 == var2 => {
                Ok(Substitution::new())
            }
            // Unify type variable with concrete type (occurs check prevents infinite types)
            (TyKind::Variable(var), ty) | (ty, TyKind::Variable(var)) => {
                if ty.occurs(*var) {
                    Err(anyhow!(
                        "Occurs check failed: {} occurs in {}",
                        var.0,
                        ty.display(cx)
                    ))
                } else {
                    let mut subst = Substitution::new();
                    subst.insert(*var, Ty(ty));
                    Ok(subst)
                }
            }
            // Primitive types unify only with themselves
            (TyKind::Int, TyKind::Int)
            | (TyKind::Float, TyKind::Float)
            | (TyKind::String, TyKind::String)
            | (TyKind::Bool, TyKind::Bool)
            | (TyKind::Duration, TyKind::Duration)
            | (TyKind::Time, TyKind::Time)
            | (TyKind::Unit, TyKind::Unit) => Ok(Substitution::new()),
            // Array types unify if their element types unify
            (TyKind::Array { inner: inner1 }, TyKind::Array { inner: inner2 }) => {
                Self::unify(cx, *inner1, *inner2)
            }
            // Function types unify if they have the same arity and corresponding types unify
            (
                TyKind::Function {
                    arg: args1,
                    ret: ret1,
                },
                TyKind::Function {
                    arg: args2,
                    ret: ret2,
                },
            ) => {
                if args1.len() != args2.len() {
                    return Err(anyhow!(
                        "Function arity mismatch: {} vs {}",
                        args1.len(),
                        args2.len()
                    ));
                }

                let mut combined_subst = Substitution::new();

                // Unify corresponding argument types
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    let subst = Self::unify(cx, *arg1, *arg2)?;
                    combined_subst = combined_subst.compose(&subst, cx);
                }

                // Apply accumulated substitutions to return types before unifying
                let substituted_ret1 = combined_subst.apply_substitution(cx, *ret1);
                let substituted_ret2 = combined_subst.apply_substitution(cx, *ret2);
                let ret_subst = Self::unify(cx, substituted_ret1, substituted_ret2)?;

                combined_subst = combined_subst.compose(&ret_subst, cx);
                Ok(combined_subst)
            }
            // All other combinations are incompatible
            _ => Err(anyhow!(
                "Cannot unify {} and {}",
                t1.display(cx),
                t2.display(cx)
            )),
        }
    }

    /// Infers the type of an expression and returns the type along with any substitutions.
    ///
    /// This is the main entry point for expression type inference. It dispatches to
    /// specialized methods based on the expression kind and accumulates type substitutions.
    pub fn infer_expr(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        env: &Environment<'cx, '_>,
        expr: &ast::ExprKind<'cx, DefaultTypeFamily>,
    ) -> Result<(Ty<'cx>, Substitution<'cx>)> {
        match expr {
            ExprKind::Literal(literal) => self.infer_literal(cx, literal),
            ExprKind::Variable(path) => self.infer_variable(cx, env, path),
            ExprKind::Binary(binary) => self.infer_binary(cx, env, binary),
            ExprKind::Unary(unary) => self.infer_unary(cx, env, unary),
            ExprKind::Apply(apply) => self.infer_apply(cx, env, apply),
            // Parenthesized expressions have the same type as their inner expression
            ExprKind::Parened(parened) => self.infer_expr(cx, env, &parened.expr),
            ExprKind::If(if_expr) => self.infer_if(cx, env, if_expr),
            _ => Err(anyhow!("Unsupported expression type")),
        }
    }

    /// Infers the type of a literal expression.
    ///
    /// Literals have well-defined types based on their syntactic form.
    /// Array literals require special handling to infer element types.
    fn infer_literal(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        literal: &ast::Literal<'cx, DefaultTypeFamily>,
    ) -> Result<(Ty<'cx>, Substitution<'cx>)> {
        let ty = match literal {
            // String literals have string type
            ast::Literal::String(_) => TyKind::String,
            // Numeric literals are typed based on their format
            ast::Literal::Numeric(numeric) => match numeric.kind {
                ast::literal::NumericKind::Integer(_) => TyKind::Int,
                ast::literal::NumericKind::Float => TyKind::Float,
            },
            // DateTime literals have time type
            ast::Literal::DateTime(_) => TyKind::Time,
            // Array literals require element type inference
            ast::Literal::Array(array) => {
                if array.exprs.is_empty() {
                    // Empty arrays get a fresh type variable for element type
                    let elem_var = TyKind::Variable(self.fresh_type_var());
                    let elem_type = cx.alloc_type(elem_var);
                    TyKind::Array { inner: elem_type }
                } else {
                    // Non-empty arrays: infer type from first element, then unify with rest
                    let (first_type, mut subst) =
                        self.infer_expr(cx, &Environment::<'cx, '_>::new(), &array.exprs[0])?;

                    // Ensure all elements have compatible types
                    for expr in array.exprs.iter().skip(1) {
                        let (expr_type, expr_subst) =
                            self.infer_expr(cx, &Environment::<'cx, '_>::new(), expr)?;
                        subst = subst.compose(&expr_subst, cx);

                        // Apply current substitutions before unifying
                        let unified_first = subst.apply_substitution(cx, first_type);
                        let unified_expr = subst.apply_substitution(cx, expr_type);
                        let unify_subst = Self::unify(cx, unified_first, unified_expr)?;
                        subst = subst.compose(&unify_subst, cx);
                    }

                    // Return array type with final element type
                    let final_elem_type = subst.apply_substitution(cx, first_type);
                    return Ok((
                        cx.alloc_type(TyKind::Array {
                            inner: final_elem_type,
                        }),
                        subst,
                    ));
                }
            }
            _ => return Err(anyhow!("Unsupported literal type")),
        };

        Ok((cx.alloc_type(ty), Substitution::new()))
    }

    /// Infers the type of a variable reference.
    ///
    /// This performs name resolution by first checking the local environment,
    /// then falling back to module-level resolution for global items.
    fn infer_variable(
        &mut self,
        _cx: &'cx TypingContext<'cx>,
        env: &Environment<'cx, '_>,
        path: &ast::Path<'cx, DefaultTypeFamily>,
    ) -> Result<(Ty<'cx>, Substitution<'cx>)> {
        let var_name = path.raw;

        // Perform name resolution - check local environment first
        if let Some(type_ref) = env.lookup_variable(var_name) {
            Ok((type_ref, Substitution::new()))
        } else {
            // If not found locally, try resolving from modules
            match self.module_loader.resolve_path(var_name) {
                Some(item) => Ok((item.ty(), Substitution::new())),
                None => Err(anyhow!("Unbound variable: {var_name}")),
            }
        }
    }

    /// Infers the type of a binary expression.
    ///
    /// Binary expressions require type checking both operands and ensuring they
    /// are compatible with the operator. Different operators have different type requirements.
    fn infer_binary(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        env: &Environment<'cx, '_>,
        binary: &ast::Binary<'cx, DefaultTypeFamily>,
    ) -> Result<(Ty<'cx>, Substitution<'cx>)> {
        // Infer types of both operands
        let (lhs_type, lhs_subst) = self.infer_expr(cx, env, &binary.lhs)?;
        let (rhs_type, rhs_subst) = self.infer_expr(cx, env, &binary.rhs)?;

        // Combine substitutions from both operands
        let combined_subst = lhs_subst.compose(&rhs_subst, cx);
        let unified_lhs = combined_subst.apply_substitution(cx, lhs_type);
        let unified_rhs = combined_subst.apply_substitution(cx, rhs_type);

        match binary.op {
            // Arithmetic operations require numeric types and return the same type
            ast::BinOp::Add(_)
            | ast::BinOp::Sub(_)
            | ast::BinOp::Mul(_)
            | ast::BinOp::Div(_)
            | ast::BinOp::Mod(_) => {
                let unify_subst = Self::unify(cx, unified_lhs, unified_rhs)?;
                let final_subst = combined_subst.compose(&unify_subst, cx);

                let result_type = final_subst.apply_substitution(cx, unified_lhs);
                match result_type.kind() {
                    TyKind::Int | TyKind::Float => Ok((result_type, final_subst)),
                    _ => Err(anyhow!(
                        "Arithmetic operation requires numeric type, got {}",
                        result_type.display(cx)
                    )),
                }
            }
            // Logical operations require boolean operands and return boolean
            ast::BinOp::And(_) | ast::BinOp::Or(_) => {
                let bool_type = cx.alloc_type(TyKind::Bool);
                let bool_unify_lhs = Self::unify(cx, unified_lhs, bool_type)?;
                let bool_unify_rhs = Self::unify(cx, unified_rhs, bool_type)?;
                let bool_combined = bool_unify_lhs.compose(&bool_unify_rhs, cx);
                let final_subst = combined_subst.compose(&bool_combined, cx);
                Ok((bool_type, final_subst))
            }
            _ => Err(anyhow!("Unsupported binary operator")),
        }
    }

    /// Infers the type of a unary expression.
    ///
    /// Unary expressions require checking the operand type and ensuring it
    /// is compatible with the unary operator.
    fn infer_unary(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        env: &Environment<'cx, '_>,
        unary: &ast::Unary<'cx, DefaultTypeFamily>,
    ) -> Result<(Ty<'cx>, Substitution<'cx>)> {
        let (expr_type, expr_subst) = self.infer_expr(cx, env, &unary.expr)?;

        match unary.op {
            // Negation requires numeric type and returns the same type
            ast::UnOp::Neg(_) => {
                let unified_type = expr_subst.apply_substitution(cx, expr_type);
                match unified_type.kind() {
                    TyKind::Int | TyKind::Float => Ok((unified_type, expr_subst)),
                    _ => Err(anyhow!(
                        "Negation requires numeric type, got {}",
                        unified_type.display(cx)
                    )),
                }
            }
            _ => Err(anyhow!("Unsupported unary operator")),
        }
    }

    fn infer_apply(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        env: &Environment<'cx, '_>,
        apply: &ast::Apply<'cx, DefaultTypeFamily>,
    ) -> Result<(Ty<'cx>, Substitution<'cx>)> {
        let (func_type, func_subst) = self.infer_expr(cx, env, &apply.function)?;

        let mut arg_types = Vec::new();
        let mut combined_subst = func_subst;

        for arg in apply.args {
            let (arg_type, arg_subst) = self.infer_expr(cx, env, arg)?;
            combined_subst = combined_subst.compose(&arg_subst, cx);
            let unified_arg_type = combined_subst.apply_substitution(cx, arg_type);
            arg_types.push(unified_arg_type);
        }

        let return_var = TyKind::Variable(self.fresh_type_var());
        let return_type = cx.alloc_type(return_var);
        let expected_func_type = cx.alloc_type(TyKind::Function {
            arg: arg_types,
            ret: return_type,
        });

        let unified_func_type = combined_subst.apply_substitution(cx, func_type);
        let unify_subst = Self::unify(cx, unified_func_type, expected_func_type)?;
        let final_subst = combined_subst.compose(&unify_subst, cx);

        let final_return_type = final_subst.apply_substitution(cx, return_type);
        Ok((final_return_type, final_subst))
    }

    fn infer_if(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        env: &Environment<'cx, '_>,
        if_expr: &ast::If<'cx, DefaultTypeFamily>,
    ) -> Result<(Ty<'cx>, Substitution<'cx>)> {
        let (cond_type, cond_subst) = self.infer_expr(cx, env, &if_expr.cond)?;
        let bool_type = cx.alloc_type(TyKind::Bool);
        let bool_unify = Self::unify(cx, cond_type, bool_type)?;
        let combined_subst = cond_subst.compose(&bool_unify, cx);

        let (then_type, then_subst) = self.infer_block(cx, env, if_expr.then_clause)?;
        let then_combined = combined_subst.compose(&then_subst, cx);

        if let Some(else_clause) = &if_expr.else_opt {
            let (else_type, else_subst) = self.infer_block(cx, env, else_clause.else_clause)?;
            let else_combined = then_combined.compose(&else_subst, cx);

            let unified_then = else_combined.apply_substitution(cx, then_type);
            let unified_else = else_combined.apply_substitution(cx, else_type);
            let branch_unify = Self::unify(cx, unified_then, unified_else)?;
            let final_subst = else_combined.compose(&branch_unify, cx);

            let result_type = final_subst.apply_substitution(cx, unified_then);
            Ok((result_type, final_subst))
        } else {
            let unit_type = cx.alloc_type(TyKind::Unit);
            let unit_unify = Self::unify(cx, then_type, unit_type)?;
            let final_subst = then_combined.compose(&unit_unify, cx);
            Ok((unit_type, final_subst))
        }
    }

    fn infer_block(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        env: &Environment<'cx, '_>,
        block: &ast::Block<'cx, DefaultTypeFamily>,
    ) -> Result<(Ty<'cx>, Substitution<'cx>)> {
        let mut local_env = env.extend_inherit();
        let mut combined_subst = Substitution::new();
        let mut last_type = cx.alloc_type(TyKind::Unit);

        for item in block.scope.items {
            match item {
                ast::ScopeItem::Row(row) => {
                    if let Some(content) = &row.content {
                        let (stmt_type, stmt_subst) =
                            self.infer_statement(cx, &local_env, content)?;
                        combined_subst = combined_subst.compose(&stmt_subst, cx);

                        if let StatementKind::Let(let_stmt) = content {
                            let var_name = let_stmt.variable.raw;
                            let identifier = Identifier {
                                name: var_name,
                                scope_depth: local_env.scope_depth(),
                            };
                            let identifier_id = cx.alloc_identifier(identifier);
                            local_env.bind(var_name, identifier_id, stmt_type);
                        } else {
                            last_type = stmt_type;
                        }
                    }
                }
                ast::ScopeItem::Block(nested_block) => {
                    let (block_type, block_subst) =
                        self.infer_block(cx, &local_env, nested_block)?;
                    combined_subst = combined_subst.compose(&block_subst, cx);
                    last_type = block_type;
                }
            }
        }

        Ok((last_type, combined_subst))
    }

    fn infer_statement(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        env: &Environment<'cx, '_>,
        stmt: &StatementKind<'cx, DefaultTypeFamily>,
    ) -> Result<(Ty<'cx>, Substitution<'cx>)> {
        match stmt {
            StatementKind::Let(let_stmt) => self.infer_expr(cx, env, &let_stmt.rhs),
            StatementKind::Expr(expr_stmt) => self.infer_expr(cx, env, &expr_stmt.expr),
            StatementKind::Return(_) => Ok((cx.alloc_type(TyKind::Unit), Substitution::new())),
        }
    }

    /// Performs type checking on an entire program.
    ///
    /// This processes all top-level definitions in the program, including
    /// functions and constants, and ensures they are well-typed.
    pub fn check_program(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        program: &ast::Program<'cx, DefaultTypeFamily>,
    ) -> Result<()> {
        // Create global environment for top-level definitions
        let mut global_env = Environment::<'cx, '_>::new();

        // Process each top-level definition
        for definition in program.definitions {
            match definition {
                ast::Definition::Function(func_def) => {
                    self.check_function(cx, &mut global_env, func_def)?;
                }
                ast::Definition::Constant(const_def) => {
                    self.check_constant(cx, &mut global_env, const_def)?;
                }
            }
        }

        Ok(())
    }

    fn check_function(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        env: &mut Environment<'cx, '_>,
        func_def: &ast::FunctionDef<'cx, DefaultTypeFamily>,
    ) -> Result<()> {
        let func_name = func_def.name.raw;

        let mut param_types = Vec::new();
        let mut func_env = env.extend_inherit();

        for param in func_def.parameters {
            let param_name = param.name.raw;
            let param_type = self.resolve_type_from_path(param.ty.raw)?;
            param_types.push(param_type);

            let param_identifier = Identifier {
                name: param_name,
                scope_depth: func_env.scope_depth(),
            };
            let param_identifier_id = cx.alloc_identifier(param_identifier);
            func_env.bind(param_name, param_identifier_id, param_type);
        }

        let (body_type, _) = self.infer_block(cx, &func_env, func_def.body)?;
        let func_type = cx.alloc_type(TyKind::Function {
            arg: param_types,
            ret: body_type,
        });

        let func_identifier = Identifier {
            name: func_name,
            scope_depth: env.scope_depth(),
        };
        let func_identifier_id = cx.alloc_identifier(func_identifier);
        env.bind(func_name, func_identifier_id, func_type);

        Ok(())
    }

    fn check_constant(
        &mut self,
        cx: &'cx TypingContext<'cx>,
        env: &mut Environment<'cx, '_>,
        const_def: &ast::ConstantDef<'cx, DefaultTypeFamily>,
    ) -> Result<()> {
        let const_name = const_def.name.raw;
        let declared_type = self.resolve_type_from_path(const_def.ty.raw)?;
        let (inferred_type, subst) = self.infer_expr(cx, env, &const_def.value)?;

        let unified_type = subst.apply_substitution(cx, inferred_type);
        let _unify_result = Self::unify(cx, declared_type, unified_type)?;

        let const_identifier = Identifier {
            name: const_name,
            scope_depth: env.scope_depth(),
        };
        let const_identifier_id = cx.alloc_identifier(const_identifier);
        env.bind(const_name, const_identifier_id, declared_type);

        Ok(())
    }

    fn resolve_type_from_path(&self, path: &str) -> Result<Ty<'cx>> {
        match self.module_loader.resolve_path(path) {
            Some(ModuleItem::Type { ty, .. }) => Ok(ty),
            Some(_) => Err(anyhow!("Path '{}' does not refer to a type", path)),
            None => Err(anyhow!("Unknown type: {}", path)),
        }
    }
}

impl<'cx> Default for TypeChecker<'cx> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_typing_context() {
        let cx = TypingContext::new();

        let int_type = cx.alloc_type(TyKind::Int);
        let array_type = cx.alloc_type(TyKind::Array { inner: int_type });

        assert_eq!(cx.display_type(int_type), "i32");
        assert_eq!(cx.display_type(array_type), "[i32]");

        let identifier = Identifier {
            name: "test",
            scope_depth: 0,
        };
        let id = cx.alloc_identifier(identifier);
        assert_eq!(id.name, "test");
    }

    #[test]
    fn test_builtin_module() {
        let cx = TypingContext::new();
        let builtin = create_builtin_module(&cx);

        assert_eq!(builtin.name(), "builtin");
        assert!(builtin.lookup_item("i32").is_some());
        assert!(builtin.lookup_item("f64").is_some());
        assert!(builtin.lookup_item("unknown").is_none());

        if let Some(ModuleItem::Type { name, ty }) = builtin.lookup_item("i32") {
            assert_eq!(name, "i32");
            assert!(matches!(ty.kind(), TyKind::Int));
        }
    }

    #[test]
    fn test_module_loader() {
        let cx = TypingContext::new();
        let builtin = create_builtin_module(&cx);

        let mut loader = ModuleLoader::new();
        loader.add_module("builtin".to_string(), builtin);

        assert!(loader.lookup_module("builtin").is_some());
        assert!(loader.lookup_module("unknown").is_none());

        assert!(loader.resolve_path("i32").is_some());
        assert!(loader.resolve_path("unknown").is_none());
    }

    #[test]
    fn test_type_checker_with_modules() {
        let cx = TypingContext::new();
        let builtin = create_builtin_module(&cx);

        let mut loader = ModuleLoader::new();
        loader.add_module("builtin".to_string(), builtin);

        let checker = TypeChecker::with_module_loader(loader);

        let i32_type = checker.resolve_type_from_path("i32").unwrap();
        assert!(matches!(i32_type.kind(), TyKind::Int));

        let unknown_result = checker.resolve_type_from_path("unknown");
        assert!(unknown_result.is_err());
    }

    #[test]
    fn test_substitution() {
        let cx = TypingContext::new();
        let var = TypeVariable(0);
        let int_type = cx.alloc_type(TyKind::Int);

        let mut subst = Substitution::new();
        subst.insert(var, int_type);

        let var_type = cx.alloc_type(TyKind::Variable(var));
        let result = subst.apply_substitution(&cx, var_type);

        assert!(matches!(result.kind(), TyKind::Int));
    }

    #[test]
    fn test_unify_basic() {
        let cx = TypingContext::new();

        let int_type1 = cx.alloc_type(TyKind::Int);
        let int_type2 = cx.alloc_type(TyKind::Int);
        let float_type = cx.alloc_type(TyKind::Float);

        let result = TypeChecker::unify(&cx, int_type1, int_type2);
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());

        let result = TypeChecker::unify(&cx, int_type1, float_type);
        assert!(result.is_err());
    }

    #[test]
    fn test_variable_resolution_priority() {
        let cx = TypingContext::new();
        let builtin = create_builtin_module(&cx);

        let mut loader = ModuleLoader::new();
        loader.add_module("builtin".to_string(), builtin);
        let mut checker = TypeChecker::with_module_loader(loader);

        let mut env = Environment::<'_, '_>::new();

        // ローカル変数 "i32" を定義（組み込み型をシャドーイング）
        let local_i32_type = cx.alloc_type(TyKind::String);
        let local_identifier = Identifier {
            name: "i32",
            scope_depth: 0,
        };
        let local_id = cx.alloc_identifier(local_identifier);
        env.bind("i32", local_id, local_i32_type);

        // パスを作成して型を解決
        let path = ast::Path {
            raw: "i32",
            segments: &[],
        };

        let (resolved_type, _) = checker.infer_variable(&cx, &env, &path).unwrap();

        // ローカル変数が優先されるべき（String型になっている）
        assert!(matches!(resolved_type.kind(), TyKind::String));
    }
}
