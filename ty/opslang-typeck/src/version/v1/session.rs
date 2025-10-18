use super::*;

use opslang_module::version::v1::path::ModulePath;

/// Entry in the module tree representing a single module.
#[derive(Debug)]
pub struct ModuleEntry<'cx> {
    /// The AST program for this module.
    pub program: ast::Program<'cx>,
    /// The IR program result (populated after type checking).
    pub ir_program: Option<ir::Program<'cx>>,
}

/// Type checking session managing multiple modules.
///
/// The session holds a tree of modules and accumulates type checking results.
pub struct Session<'cx, 'mcx, 'env> {
    /// Map from module path to module entry.
    modules: HashMap<ModulePath<'mcx>, ModuleEntry<'cx>>,
    /// Map from module path to its environment with registered signatures.
    module_environments: HashMap<ModulePath<'mcx>, Environment<'cx, 'env>>,
}

impl<'cx, 'mcx, 'env> Session<'cx, 'mcx, 'env> {
    /// Creates a new empty session.
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            module_environments: HashMap::new(),
        }
    }

    /// Adds a module to the session.
    pub fn add_module(&mut self, path: ModulePath<'mcx>, program: ast::Program<'cx>) {
        self.modules.insert(
            path,
            ModuleEntry {
                program,
                ir_program: None,
            },
        );
    }

    /// Gets a module entry by path.
    pub fn get_module(&self, path: &ModulePath<'mcx>) -> Option<&ModuleEntry<'cx>> {
        self.modules.get(path)
    }

    /// Gets a mutable module entry by path.
    pub fn get_module_mut(&mut self, path: &ModulePath<'mcx>) -> Option<&mut ModuleEntry<'cx>> {
        self.modules.get_mut(path)
    }

    /// Iterates over all module paths and entries.
    pub fn iter(&self) -> impl Iterator<Item = (&ModulePath<'mcx>, &ModuleEntry<'cx>)> {
        self.modules.iter()
    }

    /// Iterates mutably over all module paths and entries.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&ModulePath<'mcx>, &mut ModuleEntry<'cx>)> {
        self.modules.iter_mut()
    }

    /// Gets the environment for a module.
    pub fn get_environment(&self, path: &ModulePath<'mcx>) -> Option<&Environment<'cx, 'env>> {
        self.module_environments.get(path)
    }

    /// Gets a mutable environment for a module.
    pub fn get_environment_mut(
        &mut self,
        path: &ModulePath<'mcx>,
    ) -> Option<&mut Environment<'cx, 'env>> {
        self.module_environments.get_mut(path)
    }

    /// Registers an environment for a module.
    pub fn register_environment(&mut self, path: ModulePath<'mcx>, env: Environment<'cx, 'env>) {
        self.module_environments.insert(path, env);
    }
}

impl<'cx, 'mcx, 'env> Default for Session<'cx, 'mcx, 'env> {
    fn default() -> Self {
        Self::new()
    }
}
