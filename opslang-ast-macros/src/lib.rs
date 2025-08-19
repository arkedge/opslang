mod derive_map_into_token;
mod derive_position;
mod derive_span;
mod v1_default_type_subst;

#[proc_macro_derive(Span)]
pub fn derive_span(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_span::derive_span(input)
        .unwrap_or_else(std::convert::identity)
        .into()
}

#[proc_macro_derive(OrderSpan)]
pub fn derive_order_span(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_span::derive_order_span(input)
        .unwrap_or_else(std::convert::identity)
        .into()
}

#[proc_macro_derive(Position)]
pub fn derive_position(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_position::derive_position(input)
        .unwrap_or_else(std::convert::identity)
        .into()
}

#[proc_macro_derive(MapIntoToken)]
pub fn derive_map_into_token(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_map_into_token::derive_map_into_token(input)
        .unwrap_or_else(std::convert::identity)
        .into()
}

#[proc_macro]
pub fn v1_default_type_subst(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    v1_default_type_subst::v1_default_type_subst(input)
        .unwrap_or_else(std::convert::identity)
        .into()
}

#[proc_macro]
pub fn v1_default_type_subst_internal(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    v1_default_type_subst::v1_default_type_subst_internal(input)
        .unwrap_or_else(std::convert::identity)
        .into()
}
