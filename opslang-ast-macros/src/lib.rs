mod derive_position;
mod derive_span;
mod derive_trivial_bridge;

#[proc_macro_derive(TrivialBridge)]
pub fn derive_trivial_bridge(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive_trivial_bridge::derive_trivial_bridge(input)
        .unwrap_or_else(std::convert::identity)
        .into()
}

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
