use syn::DeriveInput;

pub fn derive_position(
    input: proc_macro::TokenStream,
) -> Result<proc_macro2::TokenStream, proc_macro2::TokenStream> {
    let input: DeriveInput = syn::parse(input).map_err(syn::Error::into_compile_error)?;
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
