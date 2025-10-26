use syn::{DeriveInput, Token, WherePredicate, punctuated::Punctuated, spanned::Spanned};

pub fn derive_span(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = input.ident;
    let span = quote::quote! {
        impl crate::loc::Span for #name {
            fn span(&self) -> crate::Span {
                self.span
            }
        }
    };
    Ok(span)
}

pub fn derive_order_span(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = input.ident.clone();
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let mut predicates = Punctuated::<WherePredicate, Token![,]>::new();
    if let Some(where_clause) = where_clause {
        predicates.clone_from(&where_clause.predicates);
    }
    let span = input.span();
    let data = if let syn::Data::Struct(data) = input.data {
        data
    } else {
        return Err(syn::Error::new_spanned(
            input,
            "OrderSpan can only be derived for structs",
        ));
    };
    let fields = if let syn::Fields::Named(fields) = data.fields {
        fields
    } else {
        return Err(syn::Error::new(
            span,
            "OrderSpan can only be derived for structs with named fields",
        ));
    };
    let Some(first_field) = fields.named.first() else {
        return Err(syn::Error::new_spanned(
            fields,
            "OrderSpan can only be derived for structs with at least one field",
        ));
    };
    let first_field_name = &first_field.ident.as_ref().unwrap();
    let first_field_ty = &first_field.ty;
    let last_field = fields.named.last().unwrap();
    let last_field_name = &last_field.ident.as_ref().unwrap();
    let last_field_ty = &last_field.ty;
    predicates.push(syn::parse_quote! {
        #first_field_ty: crate::loc::Span
    });
    predicates.push(syn::parse_quote! {
        #last_field_ty: crate::loc::Span
    });
    let span = quote::quote! {
        impl #impl_generics crate::loc::Span for #name #ty_generics
        where
            #predicates
        {
            fn span_start(&self) -> crate::Position {
                self.#first_field_name.span_start()
            }
            fn span_end(&self) -> crate::Position {
                self.#last_field_name.span_end()
            }
            fn span(&self) -> crate::Span {
                crate::Span {
                    start: self.span_start(),
                    end: self.span_end(),
                }
            }
        }
    };

    Ok(span)
}
