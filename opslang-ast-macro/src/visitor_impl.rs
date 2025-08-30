use crate::ast_types::AstType;
use opslang_visitor_macro_helper::no_intermediate_helper;

/// Generates visitor implementation for AST types.
pub fn visitor_impl(
    input: opslang_visitor_macro_helper::VisitorImpl,
) -> Result<proc_macro2::TokenStream, syn::Error> {
    // Get all AST types for v1 syntax
    let ast_node_types = AstType::get_v1_ast_node_types();

    // Convert AST types to VisitorType format with 'cx lifetime
    // Generate both Visit and VisitMut implementations for each type
    let visitor_types = ast_node_types
        .iter()
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

    // Note: Generic implementations are now handled separately in generate_generic_visitor_impls

    // Generate additional implementations for generic types like &[T], Option<T>, etc.
    let additional_impls = generate_adhoc_visitor_impls(&input);

    let main_expanded = opslang_visitor_macro_helper::generate_visitor_impl(input, visitor_types)?;

    Ok(quote::quote! {
        #main_expanded
        #additional_impls
    })
}

/// Generate additional Visitor implementations for generic types.
fn generate_adhoc_visitor_impls(
    visitor_impl: &opslang_visitor_macro_helper::VisitorImpl,
) -> proc_macro2::TokenStream {
    use quote::quote;

    let impl_type = &visitor_impl.impl_type;
    let impl_generics = &visitor_impl.impl_generics;
    let (_, ty_generics, _) = impl_generics.split_for_impl();

    // Add '__cx lifetime to existing generics
    let updated_generics = no_intermediate_helper::update_generics(impl_generics, |g| {
        g.params.push(syn::parse_quote!('__cx));
    });

    // Generate generic implementations
    let generic_impls = no_intermediate_helper::generate_generic_visitor_impls(
        &ty_generics,
        &updated_generics,
        impl_type,
    );

    let (updated_impl_generics, _, updated_where_clause) = updated_generics.split_for_impl();

    // Generate visitor implementations using macro that includes parsing
    macro_rules! impl_by_default {
        ($(impl $ty:ty;)*) => {
            quote! {$(
                ::opslang_visitor::impl_visitor!(#updated_impl_generics #impl_type #ty_generics [visit] $ty #updated_where_clause);
            )*}
        };
    }

    let specific_impls = impl_by_default! {
        impl ::opslang_ast::DefaultTypeFamily;
        impl ::opslang_ast::syntax::v1::Span;
        impl ::opslang_ast::syntax::v1::BytePos;
        impl ::opslang_ast::syntax::v1::literal::NumericKind;
        impl ::opslang_ast::syntax::v1::ExprKind<'__cx>;
    };

    quote! {
        #generic_impls
        #specific_impls
    }
}
