use std::ops::Deref;

/// Represents a module path as a cons cell (single-linked list).
///
/// Module paths form a chain from leaf to root, enabling efficient
/// sharing of common parent paths.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModulePathData<'cx> {
    /// The segment name at this level.
    pub segment: &'cx str,
    /// Reference to the parent path, or None for root-level modules.
    pub parent: Option<ModulePath<'cx>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModulePath<'cx>(pub(super) &'cx ModulePathData<'cx>);

impl<'cx> Deref for ModulePath<'cx> {
    type Target = &'cx ModulePathData<'cx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cx> ModulePath<'cx> {
    /// Collects all components from root to leaf.
    pub fn components(self) -> Vec<&'cx str> {
        let mut result = Vec::new();
        let mut current = Some(self);
        while let Some(path) = current {
            result.push(path.segment);
            current = path.parent;
        }
        result.reverse();
        result
    }
}
