//! IR visitor type descriptor utilities and wrapper types.
//!
//! This module contains supplementary types and utilities that support the main IR visitor
//! type registry. These include wrapper types for multi-crate path generation and utility
//! methods for working with complex type family substitutions.
//!
//! # Contents
//!
//! - **Wrapper types**: Multi-crate path generators like `OutsideIrCrateType`
//! - **Path generation**: Methods for handling AST/IR/Ty crate references
//! - **Type family utilities**: Support for type family substitution and mapping
//!
//! The core registry types (`IrNodeTy`, `IrInterTy`, etc.) are defined in the parent module,
//! while this module provides the supporting infrastructure for complex multi-crate scenarios.

use super::*;

/// A newtyped wrapper for [`IrNodeTy`] that ensures super-qualified paths.
///
/// This type generates paths like `super::Type<'cx>` or `super::module::Type<'cx>` and is
/// intended for contexts where relative references within the same crate are appropriate,
/// such as in trait declarations.
#[derive(Clone, Debug)]
pub struct InsideV1ChildModTy<'a> {
    pub(super) inner: &'a IrNodeTy,
}

impl InsideV1ChildModTy<'_> {
    /// Generate super-qualified token stream path.
    ///
    /// Returns a [`proc_macro2::TokenStream`] representing paths like `super::Type<'cx>`
    /// suitable for relative references within the same crate.
    pub fn super_path(&self) -> Path {
        self.inner.full_path(true)
    }
}

/// A newtyped wrapper for [`IrNodeTy`] that ensures crate-qualified paths.
///
/// This type generates paths like `::opslang_ir::version::v1::Type<'cx>` and is intended
/// for contexts where external crate references are needed, such as in visitor implementations
/// that reference types from outside the current crate.
#[derive(Clone, Debug)]
pub struct OutsideIrCrateTy<'a> {
    pub(super) inner: &'a IrNodeTy,
}

impl OutsideIrCrateTy<'_> {
    /// Generate fully qualified path with crate prefix.
    ///
    /// Returns a string like `::opslang_ir::version::v1::Type<'cx>` suitable for
    /// external references to IR types.
    pub fn full_crate_path(&self) -> Path {
        self.inner.full_path(false)
    }
}

/// A newtyped wrapper for [`IrInterTy`] that ensures crate-qualified paths.
#[derive(Clone, Debug)]
pub struct OutsideIrCrateInterTy<'a> {
    pub(super) inner: &'a IrInterTy,
}

impl OutsideIrCrateInterTy<'_> {
    /// Generate fully qualified path with crate prefix.
    pub fn full_path(&self) -> Cow<'_, Path> {
        self.inner.full_path()
    }
}

/// Wrapper for `InstanceKind` that provides type generation methods.
#[derive(Clone, Copy, Debug)]
pub struct IrNodeTyInstance {
    pub(super) ty: InstanceKind,
    pub(super) has_lifetime: bool,
}

/// Specifies the source crate and type family for IR node types.
#[derive(Clone, Copy, Debug)]
pub enum InstanceKind {
    /// AST crate types with optional type family substitution.
    Ast(TypeSubstitution),
    /// Native IR crate types.
    Ir,
    /// Ty crate types.
    Ty,
}

/// Controls type family substitution for AST types in IR context.
#[derive(Clone, Copy, Debug)]
pub enum TypeSubstitution {
    /// Use default TypeFamily (no substitution).
    Default,
    /// Use IrTypeFamily for IR context.
    Ir,
}

impl std::ops::Deref for IrNodeTyInstance {
    type Target = InstanceKind;

    fn deref(&self) -> &Self::Target {
        &self.ty
    }
}

impl InstanceKind {
    /// Return self for method chaining.
    fn kind(self) -> Self {
        self
    }
}

impl TypeSubstitution {
    /// Generate type family argument for generic instantiation.
    fn ty_arg(&self, rebase_ir_to_super: bool) -> Option<GenericArgument> {
        match self {
            TypeSubstitution::Default => None,
            TypeSubstitution::Ir => Some(GenericArgument::Type(if rebase_ir_to_super {
                parse_quote!(super::IrTypeFamily)
            } else {
                parse_quote!(::opslang_ir::version::v1::IrTypeFamily)
            })),
        }
    }
}

impl InstanceKind {
    /// Generate type family argument for this instance kind.
    fn ty_arg(&self, rebase_ir_to_super: bool) -> Option<GenericArgument> {
        match self {
            InstanceKind::Ast(type_substitution) => type_substitution.ty_arg(rebase_ir_to_super),
            InstanceKind::Ir => None,
            InstanceKind::Ty => None,
        }
    }
}

impl InstanceKind {
    /// Generate base crate path for this instance kind.
    fn base_path(&self, rebase_ir_to_super: bool) -> Path {
        match self {
            Self::Ir if rebase_ir_to_super => parse_quote!(super),
            Self::Ir => parse_quote!(::opslang_ir::version::v1),
            Self::Ty => parse_quote!(::opslang_ty::version::v1),
            Self::Ast(_) => parse_quote!(::opslang_ast::syntax::v1),
        }
    }
}

impl IrNodeTyInstance {
    /// Generate complete generic arguments including lifetime and type family.
    fn ty_generics(&self, rebase_ir_to_super: bool) -> AngleBracketedGenericArguments {
        let mut g = AngleBracketedGenericArguments {
            lt_token: Token![<](Span::call_site()),
            gt_token: Token![>](Span::call_site()),
            colon2_token: None,
            args: Punctuated::new(),
        };
        if self.has_lifetime {
            g.args.push(parse_quote!('cx));
        }
        if let Some(path) = self.kind().ty_arg(rebase_ir_to_super) {
            g.args.push(path);
        }
        g
    }
    /// Get base crate path from the wrapped instance kind.
    fn base_path(&self, rebase_ir_to_super: bool) -> Path {
        self.ty.base_path(rebase_ir_to_super)
    }
}

impl IrNodeTy {
    /// Generate full path for this type with proper crate and module qualification.
    ///
    /// When `rebase_ir_to_super` is true, IR crate types use "super" instead of absolute paths.
    fn full_path(&self, rebase_ir_to_super: bool) -> Path {
        let Self { name, child, ty } = self;
        let ty_generics = ty.ty_generics(rebase_ir_to_super);

        let mut path = ty.base_path(rebase_ir_to_super);
        if let Some(child) = child {
            path.segments.push(parse_quote!(#child));
        }
        path.segments.push(parse_quote!(#name #ty_generics));
        path
    }
}

impl IrNodeTy {
    /// Generate appropriate visit method name based on the type, method kind, and visitor mode.
    /// If module_path exists, generates `{prefix}_{module}_{name}[_mut]`, otherwise `{prefix}_{name}[_mut]`.
    pub fn generate_visit_method_name(
        &self,
        kind: opslang_visitor_macro_helper::MethodKind,
        mode: opslang_visitor_macro_helper::VisitorMode,
    ) -> String {
        use convert_case::{Case, Casing};

        let mut string;
        let prefix = match kind {
            opslang_visitor_macro_helper::MethodKind::Visit => "visit_",
            opslang_visitor_macro_helper::MethodKind::Super => "super_",
        };
        string = prefix.to_string();

        if let InstanceKind::Ast(TypeSubstitution::Default) = &*self.ty {
            string.push_str("ast_");
        }

        if let Some(module) = &self.child {
            string.push_str(module.to_string().as_str());
            string.push('_');
        };

        string.push_str(&self.name.to_string().to_case(Case::Snake));

        if let opslang_visitor_macro_helper::VisitorMode::VisitMut = mode {
            string.push_str("_mut");
        }

        string
    }
}

/// Represents a configurable IR type instance with optional wrapper and generics.
#[derive(Debug)]
pub struct IrInterTyInstance<P: ExecPhase = Runtime> {
    pub(super) name: Mapped<Ident, P>,
    pub(super) child: Option<Mapped<Ident, P>>,
    pub(super) ty: Mapped<type_descriptors::InstanceKind, P>,
    pub(super) has_lifetime: bool,
    pub(super) wrapper: Option<Mapped<Path, P>>,
}

impl IrInterTyInstance {
    /// Generate generic arguments for this type instance.
    fn ty_generics(&self, rebase_ir_to_super: bool) -> Option<AngleBracketedGenericArguments> {
        let mut args = Punctuated::<GenericArgument, Token![,]>::new();

        if self.has_lifetime {
            args.push(parse_quote!('cx));
        }
        if let Some(param) = self.ty.ty_arg(rebase_ir_to_super) {
            args.push(param);
        }

        if args.is_empty() {
            None
        } else {
            Some(AngleBracketedGenericArguments {
                lt_token: Token![<](Span::call_site()),
                gt_token: Token![>](Span::call_site()),
                colon2_token: None,
                args,
            })
        }
    }
}

impl IrInterTyInstance {
    /// Generate complete type path including wrapper if present.
    fn full_path(&self) -> Path {
        let Self {
            name,
            child,
            ty,
            has_lifetime: _,
            wrapper,
        } = self;
        let ty_generics = self.ty_generics(false);

        let mut path = ty.base_path(false);
        if let Some(child) = child {
            path.segments.push(parse_quote!(#child));
        }
        path.segments.push(parse_quote!(#name #ty_generics));
        if let Some(wrapper) = wrapper {
            path = parse_quote!(#wrapper<#path>);
        }
        path
    }
}

impl IrInterTy {
    /// Get the full path for this intermediate type.
    fn full_path(&self) -> Cow<'_, Path> {
        match self {
            IrInterTy::Instance(ir_inter_ty_instance) => {
                Cow::Owned(ir_inter_ty_instance.full_path())
            }
            IrInterTy::External(ext) => Cow::Borrowed(ext),
        }
    }
}
