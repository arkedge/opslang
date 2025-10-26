//! Definitions needed for `visitor_type_registry`.

use super::*;
use const_compatible::*;

/// Represents a type in the IR that can be instantiated with a lifetime.
///
/// # Invariants
///
/// This struct represents types from both `opslang-ast` and `opslang-ir` crates
/// that can be instantiated with a lifetime parameter, conventionally `'cx`.
///
/// The primary invariants are:
/// 1. The combination of `name`, `module_path`, and `crate_name` must form a valid path
///    to a type definition (e.g., `::opslang_ast::syntax::v1::Stmt` or `::opslang_ir::version::v1::Comment`).
/// 2. The referenced type must be a generic type that accepts exactly one lifetime parameter.
/// 3. The type must be instantiable when provided with the `'cx` lifetime.
///
/// This ensures that the procedural macros using this struct can correctly generate
/// code that references these lifetime-annotated IR types.
/// Represents an IR node type from AST, IR, or Ty crates that can be instantiated with a lifetime.
///
/// Can represent types from `opslang-ast`, `opslang-ir`, or `opslang-ty` crates with proper
/// type family substitution and path qualification.
#[derive(Debug)]
pub struct IrNodeTy<P: ExecPhase = Runtime> {
    pub(crate) name: Mapped<Ident, P>,
    pub(crate) child: Option<Mapped<Ident, P>>,
    pub(crate) ty: Mapped<IrNodeTyInstance, P>,
}

impl opslang_visitor_macro_helper::shared_visitor_trait::VisitableType for IrNodeTy {
    fn generate_visit_method_name(
        &self,
        kind: opslang_visitor_macro_helper::MethodKind,
        mode: opslang_visitor_macro_helper::VisitorMode,
    ) -> String {
        self.generate_visit_method_name(kind, mode)
    }

    fn full_type_path(&self) -> proc_macro2::TokenStream {
        let type_path = self.inside_of_v1_child_mod().super_path();
        quote::quote! { #type_path }
    }
}

impl IrNodeTy {
    /// Convert to a super-qualified type for relative references.
    ///
    /// This creates a type that generates paths like `super::Type<'cx>` or `super::module::Type<'cx>`
    /// for use in contexts where relative references within the same crate are appropriate.
    pub const fn inside_of_v1_child_mod(&self) -> InsideV1ChildModTy<'_> {
        InsideV1ChildModTy { inner: self }
    }

    /// Convert to a crate-qualified type for external references.
    ///
    /// This creates a type that generates paths like `::opslang_ir::version::v1::Type<'cx>`
    /// for use in contexts where the full crate path is needed.
    pub const fn outside_of_ir_crate(&self) -> OutsideIrCrateTy<'_> {
        OutsideIrCrateTy { inner: self }
    }

    /// Returns all IR node types for v1 syntax including both AST and IR specific types.
    pub fn get_v1_ir_node_types() -> impl Iterator<Item = Self> {
        visitor_type_registry::NODE_TYPES
            .iter()
            .map(|ty| ty.parse())
    }
}

/// Represents intermediate IR types that may be instance-based or external paths.
#[derive(Debug)]
pub enum IrInterTy<P: ExecPhase = Runtime> {
    /// IR type instance with configurable generics.
    Instance(IrInterTyInstance<P>),
    /// External type path (like std types).
    External(Mapped<Path, P>),
}

impl IrInterTy {
    /// Convert to a crate-qualified type for external references.
    pub const fn outside_of_ir_crate(&self) -> OutsideIrCrateInterTy<'_> {
        OutsideIrCrateInterTy { inner: self }
    }

    /// Returns all intermediate IR types for v1 syntax.
    pub fn get_v1_ir_inter_types() -> impl Iterator<Item = Self> {
        visitor_type_registry::INTER_TYPES
            .iter()
            .map(|ty| ty.parse())
    }
}

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
    /// Other crate types (e.g., opslang-ty).
    Other(&'static str),
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
            InstanceKind::Ir | InstanceKind::Other(_) => None,
        }
    }
}

impl InstanceKind {
    /// Generate base crate path for this instance kind.
    fn base_path(&self, rebase_ir_to_super: bool) -> Path {
        match self {
            Self::Ir if rebase_ir_to_super => parse_quote!(super),
            Self::Ir => parse_quote!(::opslang_ir::version::v1),
            Self::Other(crate_name) => {
                let crate_ident = Ident::new(crate_name, Span::call_site());
                parse_quote!(::#crate_ident::version::v1)
            }
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
    pub(super) ty: Mapped<InstanceKind, P>,
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

/// Functions for macro.
impl InstanceKind {
    /// Create AST instance with IR type family substitution.
    pub const fn ast_ir() -> Self {
        Self::Ast(TypeSubstitution::Ir)
    }
    /// Create AST instance with default type family.
    pub const fn ast_default() -> Self {
        Self::Ast(TypeSubstitution::Default)
    }
    /// Create IR crate instance.
    pub const fn ir() -> Self {
        Self::Ir
    }
}

/// Functions for macro.
impl TypeSubstitution {
    /// Create default type substitution.
    #[allow(dead_code)]
    pub const fn default() -> Self {
        Self::Default
    }
    /// Create IR type substitution.
    pub const fn ir() -> Self {
        Self::Ir
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ident_safe() {
        let _ir_node_types = IrNodeTy::get_v1_ir_node_types();
        let _ir_inter_types = IrInterTy::get_v1_ir_inter_types();
    }
}
