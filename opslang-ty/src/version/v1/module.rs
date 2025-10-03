use super::*;

/// Represents different kinds of items that can exist within a module.
///
/// Module items define the public interface of a module, including constants,
/// type definitions, and function definitions that can be imported by other modules.
#[derive(Debug, Clone, Visit, PartialEq)]
#[skip_all_visit]
pub enum ModuleItemDef<'cx> {
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

#[derive(Debug, Clone, Copy, PartialEq, Visit)]
/// Represents a module item definition.
pub struct ModuleItem<'cx>(pub &'cx ModuleItemDef<'cx>);

impl<'cx> std::ops::Deref for ModuleItem<'cx> {
    type Target = &'cx ModuleItemDef<'cx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug, Clone)]
/// Definition of a module containing named items.
pub struct ModuleDef<'cx> {
    /// The name of this module.
    id: Ident<'cx>,

    /// Map from item names to their definitions.
    items: HashMap<&'cx str, ModuleItemDef<'cx>>,
}

#[derive(Debug, Clone, Copy)]
/// Represents a module containing named items.
///
/// Modules provide namespacing and organization for types, functions, and constants.
/// Each module maintains a mapping from names to their corresponding items.
pub struct Module<'cx>(pub &'cx ModuleDef<'cx>);

impl<'cx> std::ops::Deref for Module<'cx> {
    type Target = &'cx ModuleDef<'cx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
/// Error indicating that a module item with the same name already exists.
///
/// FIXME: Replace with `HashMap::try_insert` once stabilized.
pub struct AlreadyDefinedError<'module, 'cx> {
    pub name: &'cx str,
    pub entry: std::collections::hash_map::OccupiedEntry<'module, &'cx str, ModuleItemDef<'cx>>,
}

impl<'cx> ModuleDef<'cx> {
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
    /// If an item with the same name already exists,
    fn add_item(&mut self, item: ModuleItemDef<'cx>) -> Result<(), AlreadyDefinedError<'_, 'cx>> {
        let id = match &item {
            ModuleItemDef::Constant { id, .. } => id,
            ModuleItemDef::Type { id, .. } => id,
            ModuleItemDef::Prc { id, .. } => id,
            ModuleItemDef::LibraryFn { id, .. } => id,
        };
        use std::collections::hash_map::Entry;
        // FIXME: Replace with `HashMap::try_insert` once stabilized.
        match self.items.entry(id.name) {
            Entry::Occupied(occupied_entry) => Err(AlreadyDefinedError {
                name: id.name,
                entry: occupied_entry,
            }),
            Entry::Vacant(vacant_entry) => {
                vacant_entry.insert(item);
                Ok(())
            }
        }
    }

    /// Creates a new constant module item.
    pub fn add_constant(
        &mut self,
        id: Ident<'cx>,
        ty: Ty<'cx>,
    ) -> Result<(), AlreadyDefinedError<'_, 'cx>> {
        self.add_item(ModuleItemDef::Constant { id, ty })
    }

    /// Creates a new type module item.
    pub fn add_type(
        &mut self,
        id: Ident<'cx>,
        ty: Ty<'cx>,
    ) -> Result<(), AlreadyDefinedError<'_, 'cx>> {
        self.add_item(ModuleItemDef::Type { id, ty })
    }

    /// Creates a new procedure module item.
    pub fn add_prc(
        &mut self,
        id: Ident<'cx>,
        ty: Ty<'cx>,
    ) -> Result<(), AlreadyDefinedError<'_, 'cx>> {
        self.add_item(ModuleItemDef::Prc { id, ty })
    }

    /// Creates a new library function module item, which can be polymorphic.
    pub fn add_library_function(
        &mut self,
        id: Ident<'cx>,
        ty: PolyTy<'cx>,
    ) -> Result<(), AlreadyDefinedError<'_, 'cx>> {
        self.add_item(ModuleItemDef::LibraryFn { id, ty })
    }

    /// Looks up an item by name within this module.
    ///
    /// Returns None if no item with the given name exists in this module.
    pub fn lookup_item(&self, name: opslang_ast::Path<'cx>) -> Option<&ModuleItemDef<'cx>> {
        self.items.get(name.to_string().as_str())
    }

    /// Returns an iterator over all items in this module.
    ///
    /// This allows iteration over module contents without exposing the internal HashMap.
    pub fn items(&self) -> impl Iterator<Item = &ModuleItemDef<'cx>> {
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
    modules: HashMap<&'cx str, Module<'cx>>,
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
    pub fn add_module(&mut self, module: Module<'cx>) {
        self.modules.insert(module.id.name, module);
    }

    /// Looks up a module by name.
    ///
    /// Returns None if no module with the given name is registered.
    pub fn lookup_module(&self, name: &str) -> Option<Module<'cx>> {
        self.modules.get(name).copied()
    }

    /// Resolves a path to a module item.
    ///
    /// Currently supports only flat paths that resolve to items in the builtin module.
    /// In the future, this will support hierarchical paths like "module::item".
    pub fn resolve_path(&self, path: opslang_ast::Path<'cx>) -> Option<ModuleItem<'cx>> {
        // Currently only supports non-hierarchical paths
        // Future enhancement: support "module::item" format
        if let Some(module) = self.modules.get("builtin") {
            module.lookup_item(path).map(ModuleItem)
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
