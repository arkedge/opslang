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
pub struct Session<'cx, 'env> {
    /// Map from module path to module entry.
    modules: HashMap<ModulePath<'cx>, ModuleEntry<'cx>>,
    /// Map from module path to its environment with registered signatures.
    module_toplevel: HashMap<ModulePath<'cx>, Scope<'cx, 'env>>,
}

impl<'cx, 'env> Session<'cx, 'env> {
    /// Creates a new empty session.
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
            module_toplevel: HashMap::new(),
        }
    }

    /// Adds a module to the session.
    pub fn add_module(&mut self, path: ModulePath<'cx>, program: ast::Program<'cx>) {
        self.modules.insert(
            path,
            ModuleEntry {
                program,
                ir_program: None,
            },
        );
    }

    /// Gets a module entry by path.
    pub fn get_module(&self, path: &ModulePath<'cx>) -> Option<&ModuleEntry<'cx>> {
        self.modules.get(path)
    }

    /// Gets a mutable module entry by path.
    pub fn get_module_mut(&mut self, path: &ModulePath<'cx>) -> Option<&mut ModuleEntry<'cx>> {
        self.modules.get_mut(path)
    }

    /// Iterates over all module paths and entries.
    pub fn iter(&self) -> impl Iterator<Item = (&ModulePath<'cx>, &ModuleEntry<'cx>)> {
        self.modules.iter()
    }

    /// Iterates mutably over all module paths and entries.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (&ModulePath<'cx>, &mut ModuleEntry<'cx>)> {
        self.modules.iter_mut()
    }

    /// Gets the environment for a module.
    pub fn get_environment(&self, path: &ModulePath<'cx>) -> Option<&Scope<'cx, '_>> {
        self.module_toplevel.get(path)
    }

    /// Gets a mutable environment for a module.
    pub fn get_environment_mut(&mut self, path: &ModulePath<'cx>) -> Option<&mut Scope<'cx, 'env>> {
        self.module_toplevel.get_mut(path)
    }

    /// Registers an environment for a module.
    pub fn register_environment(&mut self, path: ModulePath<'cx>, env: Scope<'cx, 'env>) {
        self.module_toplevel.insert(path, env);
    }
}

impl<'cx, 'env> Default for Session<'cx, 'env> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Clone, Copy)]
pub struct SecondPassSession<'cx, 'sess> {
    /// The underlying type checking session.
    sess: &'sess Session<'cx, 'sess>,

    /// The module path currently being type checked.
    pub module_path: ModulePath<'cx>,
}

impl<'cx, 'sess> SecondPassSession<'cx, 'sess> {
    pub fn new(sess: &'sess Session<'cx, 'sess>, module_path: ModulePath<'cx>) -> Self {
        Self { sess, module_path }
    }

    pub fn get_toplevel_items_of(&self, path: &ModulePath<'cx>) -> Option<&Scope<'cx, '_>> {
        self.sess.get_environment(path)
    }
}
