//! Visitor type descriptor utilities and wrapper types.
//!
//! This module contains supplementary types and utilities that support the main visitor
//! type registry. These are primarily wrapper types that provide context-specific path
//! generation and utility methods for working with the core registry types.
//!
//! # Contents
//!
//! - **Wrapper types**: Context-specific path generators like `OutsideAstCrateType`
//! - **Utility methods**: Helper functions for type descriptor manipulation  
//! - **Path generation**: Methods for creating appropriate type paths for different contexts
//!
//! The core registry types (`AstNodeType`, etc.) are defined in the parent module,
//! while this module provides the supporting infrastructure.

use super::*;

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
