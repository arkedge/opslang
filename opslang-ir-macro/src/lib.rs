mod declare_ir_visitor_trait;
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
