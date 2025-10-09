//! Definitions needed for `visitor_type_registry`.

use super::*;
use const_compatible::*;

/// Represents a generic type in the AST that can be instantiated with a lifetime.
///
/// # Invariants
///
/// This struct represents a generic type from the `opslang-ast` crate that can be
/// instantiated with a lifetime parameter, conventionally `'cx`.
///
/// The primary invariants are:
/// 1. The combination of `name` and `module_path` must form a valid path to a type
///    definition within the `opslang-ast` crate (e.g., `crate::syntax::v1::Stmt`).
/// 2. The referenced type must be a generic type that accepts exactly one lifetime
///    parameter.
/// 3. The type must be instantiable when provided with the `'cx` lifetime. For instance,
///    if the type is `path::to::TypeName`, then `path::to::TypeName<'cx>` must be a
///    valid, constructible type.
///
/// This ensures that the procedural macros using this struct can correctly generate
/// code that references these lifetime-annotated AST types.
///
/// # Context-specific types
///
/// Different usage contexts have different requirements for path generation.
/// Use the context-specific newtypes instead of accessing `full_path()` or `type_path()` directly:
/// - [`OutsideAstCrateTy`] for external crate references
/// - [`InsideV1ChildModTy`] for relative references within the same crate

#[derive(Debug)]
pub struct AstNodeTy<P: ExecPhase = Runtime> {
    pub(crate) name: Mapped<Ident, P>,
    pub(crate) child: Option<Mapped<Ident, P>>,
}

impl opslang_visitor_macro_helper::shared_visitor_trait::VisitableType for AstNodeTy {
    fn generate_visit_method_name(
        &self,
        kind: opslang_visitor_macro_helper::MethodKind,
        _mode: opslang_visitor_macro_helper::VisitorMode,
    ) -> String {
        self.generate_visit_method_name(kind)
    }

    fn full_type_path(&self) -> proc_macro2::TokenStream {
        let super_qualified = self.inside_of_v1_child_mod();
        let type_path = super_qualified.super_path();
        quote::quote! { #type_path }
    }
}

impl AstNodeTy {
    /// Convert to a crate-qualified type for external references.
    ///
    /// This creates a type that generates paths like `::opslang_ast::syntax::v1::Type<'cx>`
    /// for use in contexts where the full crate path is needed.
    pub const fn outside_of_ast_crate(&self) -> OutsideAstCrateTy<'_> {
        OutsideAstCrateTy { inner: self }
    }

    /// Convert to a super-qualified type for relative references.
    ///
    /// This creates a type that generates paths like `super::Type<'cx>` or `super::module::Type<'cx>`
    /// for use in trait declarations within the same crate.
    pub const fn inside_of_v1_child_mod(&self) -> InsideV1ChildModTy<'_> {
        InsideV1ChildModTy { inner: self }
    }

    /// Returns all AST types for v1 syntax including token types.
    pub fn get_v1_ast_node_types() -> impl Iterator<Item = Self> {
        visitor_type_registry::NODE_TYPES
            .iter()
            .map(|ty| ty.parse())
    }
}

/// Represents an intermediate AST type that may or may not have a lifetime parameter.
///
/// These are typically utility types like `Span`, `BytePos`, or `NumericKind` used
/// throughout the AST but not part of the main visitor pattern.
#[derive(Debug)]
pub struct AstInterTy<P: ExecPhase = Runtime> {
    pub(crate) name: Mapped<Ident, P>,
    pub(crate) has_lifetime: bool,
}

impl AstInterTy {
    /// Convert to a crate-qualified type for external references.
    pub const fn outside_of_ast_crate(&self) -> OutsideAstCrateInterTy<'_> {
        OutsideAstCrateInterTy { inner: self }
    }

    /// Returns all intermediate AST types for v1 syntax.
    pub fn get_v1_ast_node_types() -> impl Iterator<Item = Self> {
        visitor_type_registry::INTER_TYPES
            .iter()
            .map(|ty| ty.parse())
    }
}

/// A newtyped wrapper for [`AstNodeTy`] that ensures crate-qualified paths.
///
/// This type generates paths like `::opslang_ast::syntax::v1::Type<'cx>` and is intended
/// for contexts where external crate references are needed, such as in visitor implementations
/// that reference types from outside the current crate.
#[derive(Clone, Debug)]
pub struct OutsideAstCrateTy<'a> {
    pub(super) inner: &'a AstNodeTy,
}

impl OutsideAstCrateTy<'_> {
    /// Generate fully qualified path with crate prefix.
    ///
    /// Returns a string like `::opslang_ast::syntax::v1::Type<'cx>` suitable for
    /// external references to AST types.
    pub fn full_crate_path(&self) -> syn::Path {
        self.inner.full_path()
    }
}

/// A newtyped wrapper for [`AstNodeTy`] that ensures super-qualified paths.
///
/// This type generates paths like `super::Type<'cx>` or `super::module::Type<'cx>` and is
/// intended for contexts where relative references within the same crate are appropriate,
/// such as in trait declarations.
#[derive(Clone, Debug)]
pub struct InsideV1ChildModTy<'a> {
    pub(super) inner: &'a AstNodeTy,
}

impl InsideV1ChildModTy<'_> {
    /// Generate super-qualified token stream path.
    ///
    /// Returns a [`proc_macro2::TokenStream`] representing paths like `super::Type<'cx>`
    /// suitable for relative references within the same crate.
    pub fn super_path(&self) -> proc_macro2::TokenStream {
        self.inner.relative_path()
    }
}

/// A newtyped wrapper for [`AstInterTy`] that ensures crate-qualified paths.
#[derive(Clone, Debug)]
pub struct OutsideAstCrateInterTy<'a> {
    pub(super) inner: &'a AstInterTy,
}

impl OutsideAstCrateInterTy<'_> {
    /// Generate fully qualified path with crate prefix.
    pub fn full_path(&self) -> Path {
        self.inner.full_path()
    }
}

impl AstNodeTy {
    /// Generate full path for this type with proper module qualification.
    ///
    /// # Private
    ///
    /// This method is private to prevent direct usage. Use [`AstNodeTy::outside_of_ast_crate`]
    /// or [`AstNodeTy::inside_of_v1_child_mod`] to get context-appropriate types.
    fn full_path(&self) -> syn::Path {
        let str = match &self.child {
            Some(child) => format!("::opslang_ast::syntax::v1::{child}::{}<'cx>", self.name),
            None => format!("::opslang_ast::syntax::v1::{}<'cx>", self.name),
        };
        syn::parse_str(&str).unwrap()
    }

    /// Generate syn path for this type with proper module qualification.
    ///
    /// # Private
    ///
    /// This method is private to prevent direct usage. Use [`AstNodeTy::outside_of_ast_crate`]
    /// or [`AstNodeTy::inside_of_v1_child_mod`] to get context-appropriate types.
    fn relative_path(&self) -> proc_macro2::TokenStream {
        use quote::quote;
        let name = &self.name;
        match &self.child {
            Some(child) => {
                quote! { super::#child::#name<'cx> }
            }
            None => quote! { super::#name<'cx> },
        }
    }

    /// Generate appropriate method name based on the type and method kind.
    /// If module_path exists, generates `{prefix}_{module}_{name}`, otherwise `{prefix}_{name}`.
    pub fn generate_visit_method_name(
        &self,
        kind: opslang_visitor_macro_helper::MethodKind,
    ) -> String {
        use convert_case::{Case, Casing};

        let snake_name = self.name.to_string().to_case(Case::Snake);
        let prefix = match kind {
            opslang_visitor_macro_helper::MethodKind::Visit => "visit",
            opslang_visitor_macro_helper::MethodKind::Super => "super",
        };

        if let Some(module) = &self.child {
            let snake_module = module.to_string();
            format!("{prefix}_{snake_module}_{snake_name}")
        } else {
            format!("{prefix}_{snake_name}")
        }
    }
}

impl AstInterTy {
    /// Generate full crate path for this intermediate type.
    fn full_path(&self) -> Path {
        let name = &self.name;
        if self.has_lifetime {
            parse_quote!(::opslang_ast::syntax::v1::#name<'cx>)
        } else {
            parse_quote!(::opslang_ast::syntax::v1::#name)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ident_safe() {
        let _ast_node_types = AstNodeTy::get_v1_ast_node_types();
    }
}
