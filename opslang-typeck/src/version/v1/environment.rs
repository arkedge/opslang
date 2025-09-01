use super::{HashMap, Ident, Ty};
use opslang_ast::syntax::v1 as ast;

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
}

impl<'cx, 'env> Environment<'cx, 'env> {
    /// Creates a new top-level environment with no parent.
    ///
    /// This represents the global scope.
    pub fn new() -> Self {
        Self {
            name_bindings: HashMap::new(),
            type_bindings: HashMap::new(),
            parent: None,
        }
    }

    /// Creates a new environment that extends a parent environment.
    ///
    /// The new environment can access bindings from the parent chain while allowing local shadowing.
    pub fn extend_inherit(&'env self) -> Self {
        Self {
            name_bindings: HashMap::new(),
            type_bindings: HashMap::new(),
            parent: Some(self),
        }
    }

    /// Binds a name to an identifier and associates the identifier with a type.
    ///
    /// This is a convenience method that performs both name and type binding in one operation.
    pub fn bind(&mut self, name: &'cx str, id: Ident<'cx>, ty: Ty<'cx>) {
        self.name_bindings.insert(name, id);
        self.type_bindings.insert(id, ty);
    }

    /// Looks up a name to find its associated identifier.
    ///
    /// Searches the current environment first, then walks up the parent chain.
    /// Returns None if the name is not bound in any accessible scope.
    pub fn lookup_name(&self, name: ast::Ident<'cx>) -> Option<Ident<'cx>> {
        self.name_bindings
            .get(name.raw)
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
    pub fn lookup_variable(&self, name: ast::Ident<'cx>) -> Option<Ty<'cx>> {
        if let Some(id) = self.lookup_name(name) {
            self.lookup_type(id)
        } else {
            None
        }
    }
}

impl<'cx, 'env> Default for Environment<'cx, 'env> {
    fn default() -> Self {
        Self::new()
    }
}
