use crate::ir_types::IrType;
use opslang_visitor_macro_helper::{VisitorType, no_intermediate_helper};

/// Generates visitor implementation for IR types.
pub fn visitor_impl(
    input: proc_macro::TokenStream,
) -> Result<proc_macro2::TokenStream, proc_macro2::TokenStream> {
    let visitor_impl = syn::parse::<opslang_visitor_macro_helper::VisitorImpl>(input)
        .map_err(|e| e.to_compile_error())?;

    // Get all IR types for v1 syntax (includes both AST and IR specific types)
    let all_ir_types = IrType::get_v1_ir_types();

    // Convert IR types to VisitorType format with 'cx lifetime
    // Generate both Visit and VisitMut implementations for each type
    let mut visitor_types: Vec<VisitorType> = Vec::new();

    for ir_type in all_ir_types.iter() {
        let crate_qualified = ir_type.outside_of_ir_crate();
        let full_path = crate_qualified.full_crate_path();
        let path: syn::Path = syn::parse_str(&full_path)
            .unwrap_or_else(|e| panic!("Failed to parse path '{full_path}': {e}"));
        // Generate Visit version
        let mode = opslang_visitor_macro_helper::VisitorMode::Visit;
        visitor_types.push(VisitorType {
            generics: syn::parse_quote!(<'cx>),
            path: path.clone(),
            visit_method_name: ir_type.generate_visit_method_name(mode),
            mode,
        });

        // Generate VisitMut version
        let mode = opslang_visitor_macro_helper::VisitorMode::VisitMut;
        visitor_types.push(VisitorType {
            generics: syn::parse_quote!(<'cx>),
            path,
            visit_method_name: ir_type.generate_visit_method_name(mode),
            mode,
        });
    }

    // Generate additional implementations for generic types like &[T], Option<T>, etc.
    let additional_impls = generate_adhoc_visitor_impls(&visitor_impl);

    let main_expanded =
        opslang_visitor_macro_helper::generate_visitor_impl(visitor_impl, visitor_types)
            .map_err(|e| e.to_compile_error())?;

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

    // Generate generic implementations for both Visitor and VisitorMut
    let visit_generic_impls = no_intermediate_helper::generate_generic_visitor_impls(
        &ty_generics,
        &updated_generics,
        impl_type,
    );

    let visit_mut_generic_impls = no_intermediate_helper::generate_generic_visitor_mut_impls(
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
                ::opslang_visitor::impl_visitor!(#updated_impl_generics #impl_type #ty_generics [visit_mut] $ty #updated_where_clause);
            )*}
        };
    }

    let specific_impls = impl_by_default! {
        impl ::opslang_ast::syntax::v1::DefaultTypeFamily;
        impl ::opslang_ast::syntax::v1::Span;
        impl ::opslang_ast::syntax::v1::BytePos;
        impl ::opslang_ast::syntax::v1::literal::NumericKind;
        impl ::opslang_ast::syntax::v1::ExprKind<'__cx, ::opslang_ir::version::v1::IrTypeFamily>;
        impl ::opslang_ir::version::v1::IrTypeFamily;
        impl ::opslang_ir::version::v1::NumericKind;
        impl ::opslang_ty::version::v1::Ident<'__cx>;
        impl ::opslang_ty::version::v1::TyKind<'__cx>;
        impl ::opslang_ty::version::v1::Identifier<'__cx>;
        impl ::opslang_ty::version::v1::TypeVariable;
        impl ::std::convert::Infallible;
        impl chrono::DateTime<chrono::Utc>;
        impl usize;
        impl Vec<::opslang_ty::version::v1::Ty<'__cx>>;
    };

    quote! {
        #visit_generic_impls
        #visit_mut_generic_impls
        #specific_impls
    }
}
