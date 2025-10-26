use opslang_type_registry::v1::ir::types::{IrInterTy, IrNodeTy};
use opslang_visitor_macro_helper::{CallsiteTraitName, VisitorType, no_intermediate_helper};

/// Generates visitor implementation for IR types.
///
/// # Design Note
///
/// **You should rarely need to modify this function.** Most visitor changes happen in
/// [`visitor_type_registry.rs`](./visitor_type_registry.rs) by updating the multi-crate type registries. This function
/// mechanically converts the registered types into visitor trait implementations.
pub fn visitor_impl(
    input: opslang_visitor_macro_helper::VisitorImpl,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    // Get all IR types for v1 syntax (includes both AST and IR specific types)
    let all_ir_types = IrNodeTy::get_v1_ir_node_types();

    // Convert IR types to VisitorType format with 'cx lifetime
    // Generate both Visit and VisitMut implementations for each type
    let mut visitor_types: Vec<VisitorType> = Vec::new();

    for ir_type in all_ir_types {
        let crate_qualified = ir_type.outside_of_ir_crate();
        let path = crate_qualified.full_crate_path();
        let kind = opslang_visitor_macro_helper::MethodKind::Visit;
        // Generate Visit version
        let mode = opslang_visitor_macro_helper::VisitorMode::Visit;
        visitor_types.push(VisitorType {
            generics: syn::parse_quote!(<'cx>),
            path: path.clone(),
            visit_method_name: ir_type.generate_visit_method_name(kind, mode),
            mode,
        });

        // Generate VisitMut version
        let mode = opslang_visitor_macro_helper::VisitorMode::VisitMut;
        visitor_types.push(VisitorType {
            generics: syn::parse_quote!(<'cx>),
            path,
            visit_method_name: ir_type.generate_visit_method_name(kind, mode),
            mode,
        });
    }

    // Generate intermediate implementations for generic types like &[T], Option<T>, etc.
    let additional_impls = generate_intermediate_visitor_impls(&input);

    let callsite_trait = CallsiteTraitName::both(
        syn::parse_quote!(::opslang_ir::version::v1::visit::IrVisitor),
        syn::parse_quote!(::opslang_ir::version::v1::visit::IrMutVisitor),
    );
    let main_expanded =
        opslang_visitor_macro_helper::generate_visitor_impl(input, callsite_trait, visitor_types)?;

    Ok(quote::quote! {
        #main_expanded
        #additional_impls
    })
}

/// Generate intermediate visitor implementations for both Visit and VisitMut modes.
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

    // Generate generic implementations for both Visitor and VisitorMut
    let visit_generic_impls =
        no_intermediate_helper::generate_generic_visitor_impls(&updated_generics, impl_type);

    let visit_mut_generic_impls =
        no_intermediate_helper::generate_generic_visitor_mut_impls(&updated_generics, impl_type);

    let (updated_impl_generics, _, updated_where_clause) = updated_generics.split_for_impl();

    let specific_impls = IrInterTy::get_v1_ir_inter_types().map(|ty| {
        let outside = ty.outside_of_ir_crate();
        let ty = outside.full_path();
        let ty = &ty;
        quote! {
            ::opslang_visitor::impl_visitor!(#updated_impl_generics #impl_type [visit] #ty #updated_where_clause);
            ::opslang_visitor::impl_visitor!(#updated_impl_generics #impl_type [visit_mut] #ty #updated_where_clause);
        }
    });

    quote! {
        #visit_generic_impls
        #visit_mut_generic_impls
        #(#specific_impls)*
    }
}
