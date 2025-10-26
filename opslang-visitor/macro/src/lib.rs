mod derive_visit;

#[proc_macro_derive(Visit, attributes(skip_visit, skip_all_visit))]
pub fn derive_visit(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_visit::derive_visit(input)
        .unwrap_or_else(std::convert::identity)
        .into()
}
