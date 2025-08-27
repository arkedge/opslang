mod ir_types;
mod visitor_impl;

use proc_macro::TokenStream;

#[proc_macro]
pub fn visitor_impl(input: TokenStream) -> TokenStream {
    visitor_impl::visitor_impl(input)
        .unwrap_or_else(std::convert::identity)
        .into()
}
