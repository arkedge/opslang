mod declare_ir_visitor_trait;
mod ir_consistency_check;
mod ir_types;
mod visitor_impl;

use proc_macro::TokenStream;

#[inline]
fn wrap_proc_macro<T: syn::parse::Parse>(
    input: proc_macro::TokenStream,
    f: impl Fn(T) -> syn::Result<proc_macro2::TokenStream>,
) -> proc_macro::TokenStream {
    syn::parse(input)
        .and_then(f)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn visitor_impl(input: TokenStream) -> TokenStream {
    wrap_proc_macro(input, visitor_impl::visitor_impl)
}

/// Generates a comprehensive IR visitor trait for all V1 IR types.
///
/// This attribute macro creates immutable visitor traits that can traverse IR structures.
/// It provides `visit_*` and `super_*` method pairs for each IR type, where the `visit_*`
/// methods can be overridden by implementations and `super_*` methods provide default
/// traversal behavior.
///
/// # Usage
///
/// ```ignore
/// #[opslang_ir_macro::v1_declare_ir_visitor_trait]
/// pub trait MyIrVisitor {
///     // trait body will be automatically generated
/// }
/// ```
///
/// This generates methods like:
/// - `visit_program(&mut self, node: &Program<'cx>)`
/// - `super_program(&mut self, node: &Program<'cx>)`
/// - `visit_expr(&mut self, node: &Expr<'cx>)`
/// - `super_expr(&mut self, node: &Expr<'cx>)`
/// - And so on for all IR types...
#[proc_macro_attribute]
pub fn v1_declare_ir_visitor_trait(_attr: TokenStream, input: TokenStream) -> TokenStream {
    wrap_proc_macro(input, declare_ir_visitor_trait::declare_ir_visitor_trait)
}

/// Generates a comprehensive mutable IR visitor trait for all V1 IR types.
///
/// This attribute macro creates mutable visitor traits that can traverse and modify IR structures.
/// It provides `visit_*_mut` and `super_*_mut` method pairs for each IR type, where the `visit_*_mut`
/// methods can be overridden by implementations and `super_*_mut` methods provide default
/// traversal behavior.
///
/// # Usage
///
/// ```ignore
/// #[opslang_ir_macro::v1_declare_ir_visitor_mut_trait]
/// pub trait MyIrVisitorMut {
///     // trait body will be automatically generated
/// }
/// ```
///
/// This generates methods like:
/// - `visit_program_mut(&mut self, node: &mut Program<'cx>)`
/// - `super_program_mut(&mut self, node: &mut Program<'cx>)`
/// - `visit_expr_mut(&mut self, node: &mut Expr<'cx>)`
/// - `super_expr_mut(&mut self, node: &mut Expr<'cx>)`
/// - And so on for all IR types...
#[proc_macro_attribute]
pub fn v1_declare_ir_visitor_mut_trait(_attr: TokenStream, input: TokenStream) -> TokenStream {
    wrap_proc_macro(
        input,
        declare_ir_visitor_trait::declare_ir_visitor_mut_trait,
    )
}

/// Generates a compile-time consistency check for IR types registry.
///
/// This procedural macro verifies that all IR types defined in the centralized registry
/// (`ir_types.rs`) actually exist and are accessible from the context where this macro is called.
/// The macro is designed to be called from within a v1 child module context to ensure that
/// all registered types can be referenced using `super::` paths.
///
/// # Purpose
///
/// The IR types registry in `ir_types.rs` maintains a comprehensive list of all IR-related
/// node types from multiple crates (opslang-ast, opslang-ir, opslang-ty) for use by
/// procedural macros, but this registry is independent of the actual type definitions. This
/// creates a potential inconsistency where the registry might reference types that don't exist
/// or have been renamed/moved.
///
/// # Implementation
///
/// The macro generates compile-time checks in the form:
/// ```ignore
/// const _: () = {
///     fn check<'cx>() {
///         let _: super::Program<'cx, super::IrTypeFamily>;
///         let _: super::Expr<'cx>;
///         let _: super::ResolvedPath<'cx>;
///         // ... for each registered type
///     }
/// };
/// ```
///
/// If any type in the registry doesn't exist or isn't accessible with the expected path,
/// compilation will fail with a clear error message pointing to the problematic type.
///
/// # Usage
///
/// This macro should be called from within a child module of `opslang_ir::version::v1`
/// to verify type accessibility:
///
/// ```ignore
/// // In opslang-ir/src/version/v1/some_child_module.rs
/// opslang_ir_macro::ir_consistency_check!();
/// ```
///
/// # Guarantees
///
/// Successful compilation of this macro ensures:
/// 1. All types in the IR registry exist across referenced crates
/// 2. All types are accessible from v1 child module context using appropriate paths
/// 3. All types accept the expected lifetime and type family parameters
/// 4. The registry is consistent with actual type definitions across multiple crates
#[proc_macro]
pub fn ir_consistency_check(_input: TokenStream) -> TokenStream {
    ir_consistency_check::ir_consistency_check().into()
}
