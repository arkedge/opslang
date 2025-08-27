fn has_skip_visit(field: &syn::Field) -> bool {
    field
        .attrs
        .iter()
        .any(|attr| attr.path().is_ident("skip_visit"))
}

pub fn derive_visit(
    input: proc_macro::TokenStream,
) -> Result<proc_macro2::TokenStream, proc_macro2::TokenStream> {
    let derive_input =
        syn::parse::<syn::DeriveInput>(input).map_err(syn::Error::into_compile_error)?;
    let mut s =
        synstructure::Structure::try_new(&derive_input).map_err(syn::Error::into_compile_error)?;
    s.bind_with(|_| synstructure::BindStyle::Move);
    let visit_body = s.each(|bi| {
        if has_skip_visit(bi.ast()) {
            quote::quote! {}
        } else {
            quote::quote! {
                ::opslang_visitor::Visitor::visit(visitor, #bi);
            }
        }
    });

    let visit_mut_body = s.each(|bi| {
        if has_skip_visit(bi.ast()) {
            quote::quote! {}
        } else {
            quote::quote! {
                ::opslang_visitor::VisitorMut::visit_mut(visitor, #bi);
            }
        }
    });

    // Collect unique types of all binding fields to generate where clauses
    // Skip fields with #[skip_visit] attribute
    let mut field_types = std::collections::HashSet::new();
    for variant in s.variants() {
        for binding in variant.bindings() {
            let field = binding.ast();
            if !has_skip_visit(field) {
                field_types.insert(&field.ty);
            }
        }
    }

    // Generate impl trait bounds for Visitor trait
    let visit_bounds = field_types.iter().map(|ty| {
        quote::quote! {
            ::opslang_visitor::Visitor<#ty>
        }
    });

    // Generate impl trait bounds for VisitorMut trait
    let visit_mut_bounds = field_types.iter().map(|ty| {
        quote::quote! {
            ::opslang_visitor::VisitorMut<#ty>
        }
    });

    // Build generics properly by adding V to the existing generics
    let mut visit_generics = derive_input.generics.clone();
    visit_generics.params.push(syn::parse_quote! { V });

    let mut visit_mut_generics = derive_input.generics.clone();
    visit_mut_generics.params.push(syn::parse_quote! { V });

    // Add where clauses for visitor bounds
    visit_generics
        .make_where_clause()
        .predicates
        .push(syn::parse_quote! { V: #(#visit_bounds)+* });

    visit_mut_generics
        .make_where_clause()
        .predicates
        .push(syn::parse_quote! { V: #(#visit_mut_bounds)+* });

    let (visit_impl_generics, _, visit_where_clause) = visit_generics.split_for_impl();
    let (visit_mut_impl_generics, _, visit_mut_where_clause) = visit_mut_generics.split_for_impl();

    let (_, ty_generics, _) = derive_input.generics.split_for_impl();
    let name = &derive_input.ident;

    let result = quote::quote! {
        impl #visit_impl_generics ::opslang_visitor::TemplateVisit<V> for #name #ty_generics
        #visit_where_clause
        {
            fn super_visit(&self, visitor: &mut V) {
                match self { #visit_body }
            }
        }

        impl #visit_mut_impl_generics ::opslang_visitor::TemplateVisitMut<V> for #name #ty_generics
        #visit_mut_where_clause
        {
            fn super_visit_mut(&mut self, visitor: &mut V) {
                match self { #visit_mut_body }
            }
        }
    };

    Ok(result)
}
