mod ir_types;
mod visitor_impl;

use proc_macro::TokenStream;

#[inline]
fn wrap_proc_macro<T: syn::parse::Parse>(
    input: proc_macro::TokenStream,
    f: impl Fn(T) -> syn::Result<proc_macro2::TokenStream>,
) -> proc_macro::TokenStream {
    syn::parse(input)
        .and_then(f)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn visitor_impl(input: TokenStream) -> TokenStream {
    wrap_proc_macro(input, visitor_impl::visitor_impl)
}
