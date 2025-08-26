use crate::{ast_types::AstType, no_intermediate_helper};

/// Generates visitor implementation for AST types.
pub fn visitor_impl(
    input: proc_macro::TokenStream,
) -> Result<proc_macro2::TokenStream, proc_macro2::TokenStream> {
    let visitor_impl = syn::parse::<opslang_visitor_macro_helper::VisitorImpl>(input)
        .map_err(|e| e.to_compile_error())?;

    // Get all AST types for v1 syntax
    let all_ast_types = AstType::get_v1_ast_types();

    // Convert AST types to VisitorType format with 'cx lifetime
    let visitor_types: Vec<opslang_visitor_macro_helper::VisitorType> = all_ast_types
        .iter()
        .map(|ast_type| {
            let crate_qualified = ast_type.outside_of_ast_crate();
            let full_path = crate_qualified.full_crate_path();
            let path = syn::parse_str(&full_path)
                .unwrap_or_else(|e| panic!("Failed to parse path '{full_path}': {e}"));
            let visit_method_name = ast_type.generate_visit_method_name();

            opslang_visitor_macro_helper::VisitorType {
                generics: syn::parse_quote!(<'cx>),
                path,
                visit_method_name,
            }
        })
        .collect();

    // Note: Generic implementations are now handled separately in generate_generic_visitor_impls

    // Generate additional implementations for generic types like &[T], Option<T>, etc.
    let additional_impls = generate_additional_visitor_impls(&visitor_impl);

    let main_expanded =
        opslang_visitor_macro_helper::generate_visitor_impl(visitor_impl, visitor_types);

    Ok(quote::quote! {
        #main_expanded
        #additional_impls
    })
}

/// Generate additional Visitor implementations for generic types.
fn generate_additional_visitor_impls(
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
