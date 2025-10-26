use syn::DeriveInput;

pub fn derive_position(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
    let position = quote::quote! {
        impl #impl_generics crate::loc::Position for #name #ty_generics #where_clause {
            fn position(&self) -> crate::Position {
                self.position
            }
        }
    };
    Ok(position)
}
