mod ast_types;
mod declare_ast_visitor;
mod derive_map_into_token;
mod derive_position;
mod derive_span;
mod v1_default_type_subst;
mod visitor_impl;

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

#[proc_macro_derive(Span)]
pub fn derive_span(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    wrap_proc_macro(input, derive_span::derive_span)
}

#[proc_macro_derive(OrderSpan)]
pub fn derive_order_span(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    wrap_proc_macro(input, derive_span::derive_order_span)
}

#[proc_macro_derive(Position)]
pub fn derive_position(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    wrap_proc_macro(input, derive_position::derive_position)
}

#[proc_macro_derive(MapIntoToken)]
pub fn derive_map_into_token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    wrap_proc_macro(input, derive_map_into_token::derive_map_into_token)
}

#[proc_macro]
pub fn v1_default_type_subst(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    wrap_proc_macro(input, v1_default_type_subst::v1_default_type_subst)
}

#[proc_macro]
pub fn v1_default_type_subst_internal(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    wrap_proc_macro(input, v1_default_type_subst::v1_default_type_subst_internal)
}

/// Generates comprehensive AST visitor implementations for v1 syntax nodes.
///
/// This procedural macro implements the visitor pattern for AST traversal by generating
/// `Visitor<T>` trait implementations for all v1 AST node types. It enables differential
/// programming where you can provide custom implementations for specific node types while
/// automatically getting default traversal behavior for all others.
///
/// # Implementation Details
///
/// The macro generates `Visitor<NodeType>` implementations for every AST node type defined
/// in the v1 syntax. For methods you provide, it uses your custom implementation exactly.
/// For methods you don't provide, it generates default implementations that automatically
/// traverse child nodes. Additionally, your visitor type will implement the `AstVisitor`
/// trait, which provides convenient `visit_*` and `super_*` method pairs.
///
/// # Syntax
///
/// ```ignore (illustrative)
/// opslang_ast_macro::v1_ast_visitor_impl!(for YourVisitor {
///     fn visit_some_node(&mut self, node: &SomeNode<'cx>) {
///         // Your custom logic here
///         self.super_some_node(node); // Continue traversal
///     }
/// });
/// ```
///
/// # Method Pairs
///
/// The `AstVisitor` trait provides two methods for each AST node type:
/// - `visit_*`: Entry point for visiting a node (delegates to `Visitor<T>::visit`)
/// - `super_*`: Default traversal behavior that visits all child nodes
///
/// This design allows you to easily override specific node handling while preserving
/// automatic traversal of the entire AST structure.
///
/// # Example
///
/// ```ignore (illustrative)
/// #[derive(Default)]
/// struct CountingVisitor {
///     function_count: usize,
/// }
///
/// opslang_ast_macro::v1_ast_visitor_impl!(for CountingVisitor {
///     fn visit_function_def(&mut self, node: &opslang_ast::v1::FunctionDef<'cx>) {
///         self.function_count += 1;
///         self.super_function_def(node); // Continue visiting child nodes
///     }
/// });
/// ```
#[proc_macro]
pub fn v1_ast_visitor_impl(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    wrap_proc_macro(input, visitor_impl::visitor_impl)
}

#[proc_macro_attribute]
pub fn v1_declare_ast_visitor_trait(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    wrap_proc_macro(input, declare_ast_visitor::declare_ast_visitor_trait)
}
