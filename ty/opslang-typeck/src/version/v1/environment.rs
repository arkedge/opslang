use super::*;
use opslang_ast::syntax::v1 as ast;
use opslang_module::version::v1::ModulePath;

/// Represents a lexical environment for name and type bindings.
///
/// Environments form a chain through parent references, enabling proper
/// lexical scoping where inner scopes can shadow outer scope bindings.
#[derive(Debug)]
pub struct Scope<'cx, 'scope> {
    /// Maps variable names to their unique identifiers.
    name_bindings: HashMap<&'cx str, TypedIdent<'cx>>,

    /// Reference to parent environment for scope chaining.
    parent: Option<&'scope Self>,
}

#[derive(Debug)]
pub struct Environment<'cx, 'scope> {
    path: ModulePath<'cx>,
    scope: Scope<'cx, 'scope>,
}

impl<'cx, 'scope> DerefMut for Environment<'cx, 'scope> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.scope
    }
}

impl<'cx, 'scope> Deref for Environment<'cx, 'scope> {
    type Target = Scope<'cx, 'scope>;

    fn deref(&self) -> &Self::Target {
        &self.scope
    }
}

#[derive(Debug, Clone, Copy)]
pub struct TypedIdent<'cx> {
    pub id: Ident<'cx>,
    pub ty: Ty<'cx>,
}

impl<'cx, 'scope> Scope<'cx, 'scope> {
    /// Creates a new top-level environment with no parent.
    ///
    /// This represents the global scope.
    pub fn new() -> Self {
        Self {
            name_bindings: HashMap::new(),
            parent: None,
        }
    }

    /// Creates a new environment that extends a parent environment.
    ///
    /// The new environment can access bindings from the parent chain while allowing local shadowing.
    pub fn extend_inherit(&'scope self) -> Self {
        Self {
            name_bindings: HashMap::new(),
            parent: Some(self),
        }
    }

    /// Binds a name to an identifier and associates the identifier with a type.
    ///
    /// This is a convenience method that performs both name and type binding in one operation.
    pub fn bind(&mut self, name: ast::Ident<'cx>, id: Ident<'cx>, ty: Ty<'cx>) {
        self.name_bindings.insert(name.raw, TypedIdent { id, ty });
    }

    /// Looks up a name to find its associated identifier.
    ///
    /// Searches the current environment first, then walks up the parent chain.
    /// Returns None if the name is not bound in any accessible scope.
    pub fn lookup_var(&self, name: ast::Ident<'cx>) -> Option<TypedIdent<'cx>> {
        self.name_bindings
            .get(name.raw)
            .copied()
            .or_else(|| self.parent.and_then(|parent| parent.lookup_var(name)))
    }
}

impl<'cx, 'scope> Environment<'cx, 'scope> {
    pub fn new(path: ModulePath<'cx>, scope: Scope<'cx, 'scope>) -> Self {
        Self { scope, path }
    }

    pub fn from_path(path: ModulePath<'cx>) -> Self {
        Self {
            scope: Scope::new(),
            path,
        }
    }

    /// Creates a new environment that extends a parent environment.
    ///
    /// The new environment can access bindings from the parent chain while allowing local shadowing.
    pub fn extend_inherit(&'scope self) -> Self {
        Self {
            scope: self.scope.extend_inherit(),
            path: self.path,
        }
    }
}

impl<'cx, 'scope> Default for Scope<'cx, 'scope> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'cx> TypeChecker<'cx> {
    /// Binds a name to a new identifier with type and returns the identifier.
    pub fn bind(
        &mut self,
        scope: &mut Scope<'cx, '_>,
        name: ast::Ident<'cx>,
        ty: Ty<'cx>,
    ) -> Ident<'cx> {
        let id = self.tcx.alloc_identifier(name);
        scope.bind(name, id, ty);
        id
    }
}
