use crate::visitor_type_registry::{AstInterTy, AstNodeTy};
use opslang_visitor_macro_helper::{CallsiteTraitName, no_intermediate_helper};

/// Generates visitor implementation for AST types.
///
/// # Design Note
///
/// **You should rarely need to modify this function.** Most visitor changes happen in
/// [`visitor_type_registry.rs`](./visitor_type_registry.rs) by updating the type registries. This function
/// mechanically converts the registered types into visitor trait implementations.
pub fn visitor_impl(
    input: opslang_visitor_macro_helper::VisitorImpl,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    // Get all AST types for v1 syntax
    let ast_node_types = AstNodeTy::get_v1_ast_node_types();

    // Convert AST types to VisitorType format with 'cx lifetime
    // Generate both Visit and VisitMut implementations for each type
    let visitor_types = ast_node_types
        .map(|ast_type| {
            let crate_qualified = ast_type.outside_of_ast_crate();
            let path = crate_qualified.full_crate_path();
            let visit_method_name = ast_type
                .generate_visit_method_name(opslang_visitor_macro_helper::MethodKind::Visit);

            // Generate Visit version
            opslang_visitor_macro_helper::VisitorType {
                generics: syn::parse_quote!(<'cx>),
                path,
                visit_method_name,
                mode: opslang_visitor_macro_helper::VisitorMode::Visit,
            }
        })
        .collect();

    // Generate intermediate implementations for generic types like &[T], Option<T>, etc.
    let additional_impls = generate_intermediate_visitor_impls(&input);

    let callsite_trait = CallsiteTraitName::visitor_only(syn::parse_quote!(
        ::opslang_ast::syntax::v1::visit::AstVisitor
    ));
    let main_expanded =
        opslang_visitor_macro_helper::generate_visitor_impl(input, callsite_trait, visitor_types)?;

    Ok(quote::quote! {
        #main_expanded
        #additional_impls
    })
}

/// Generate intermediate visitor implementations for generic types like `&[T]`, `Option<T>`.
fn generate_intermediate_visitor_impls(
    visitor_impl: &opslang_visitor_macro_helper::VisitorImpl,
) -> proc_macro2::TokenStream {
    use quote::quote;

    let impl_type = &visitor_impl.impl_type;
    let impl_generics = &visitor_impl.impl_generics;

    // Add 'cx lifetime to existing generics
    let updated_generics = no_intermediate_helper::update_generics(impl_generics, |g| {
        g.params.push(syn::parse_quote!('cx));
    });

    // Generate generic implementations
    let generic_impls =
        no_intermediate_helper::generate_generic_visitor_impls(&updated_generics, impl_type);

    let (updated_impl_generics, _, updated_where_clause) = updated_generics.split_for_impl();

    let specific_impls = AstInterTy::get_v1_ast_node_types().map(|ty| {
        let outside = ty.outside_of_ast_crate();
        let ty = outside.full_path();
        let ty = &ty;
        quote! {
            ::opslang_visitor::impl_visitor!(#updated_impl_generics #impl_type [visit] #ty #updated_where_clause);
        }
    });

    quote! {
        #generic_impls
        #(#specific_impls)*
    }
}
