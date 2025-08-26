pub fn update_generics(
    impl_generics: &syn::Generics,
    update: impl Fn(&mut syn::Generics),
) -> syn::Generics {
    let mut updated_generics = impl_generics.clone();
    update(&mut updated_generics);

    // Ensure we have angle bracket tokens if we have parameters
    if updated_generics.lt_token.is_none() {
        updated_generics.lt_token = Some(syn::Token![<](proc_macro2::Span::call_site()));
        updated_generics.gt_token = Some(syn::Token![>](proc_macro2::Span::call_site()));
    }
    updated_generics
}

/// Generate generic visitor implementations from the given generics.
///
/// This function creates visitor implementations for common generic types like `str`, `Option<T>`,
/// `[T]`, `&T`, and tuple `(T1, T2)` using the provided generics as the base.
///
/// # Arguments
///
/// * `ty_generics`:
///   Type generics from the original implementation generics, used for the visitor type.
///   This should be obtained from `split_for_impl()` on the original generics.
/// * `updated_generics`:
///   The base generics that have been updated (e.g., with additional lifetimes).
///   Must have generic angle brackets (lt_token must be Some) as verified by the assert.
///   New type parameters will be added to this base for each generated implementation.
/// * `impl_type`:
///   The visitor type being implemented (e.g., `MyVisitor<'a>`).
///
/// # Generated implementations
///
/// The function generates the following visitor implementations:
/// - `str` - Uses the base generics unchanged
/// - `Option<T>` - Adds type parameter `T` with `Self: Visitor<T>` constraint
/// - `[T]` - Adds type parameter `T` with `Self: Visitor<T>` constraint  
/// - `&T` - Adds type parameter `T` with `T: ?Sized` and `Self: Visitor<T>` constraints
/// - `(T1, T2)` - Adds type parameters `T1, T2` with `Self: Visitor<T1> + Visitor<T2>` constraints
///
/// # Panics
///
/// Panics if `updated_generics.lt_token` is `None`, indicating the generics don't have angle brackets.
pub fn generate_generic_visitor_impls(
    ty_generics: &syn::TypeGenerics,
    updated_generics: &syn::Generics,
    impl_type: &syn::Type,
) -> proc_macro2::TokenStream {
    use quote::quote;

    assert!(updated_generics.lt_token.is_some());
    let (impl_generics, _, where_clause) = updated_generics.split_for_impl();

    // Base implementation for str (no additional generics)
    let str_impl = quote! {
        ::opslang_visitor::impl_visitor!(#impl_generics #impl_type #ty_generics [visit] str #where_clause);
    };

    // Add T to generics for Option<T> and [T]
    let mut t_generics = update_generics(updated_generics, |g| g.params.push(syn::parse_quote!(T)));

    // Add where clause for T
    let t_where_clause_with_visitor = t_generics.make_where_clause();
    t_where_clause_with_visitor
        .predicates
        .push(syn::parse_quote!(Self: ::opslang_visitor::Visitor<T>));
    let (t_impl_generics, _, t_where_clause) = t_generics.split_for_impl();

    let generic_impls = quote! {
        ::opslang_visitor::impl_visitor!(#t_impl_generics #impl_type #ty_generics [visit] Option<T> #t_where_clause);
        ::opslang_visitor::impl_visitor!(#t_impl_generics #impl_type #ty_generics [visit] [T] #t_where_clause);
    };

    // Generate the implementation by manually constructing the generics tokens
    let ref_t_generics = update_generics(&t_generics, |g| {
        g.make_where_clause()
            .predicates
            .push(syn::parse_quote!(T: ?Sized))
    });

    let (ref_t_impl_generics, _, ref_t_where_clause) = ref_t_generics.split_for_impl();

    let ref_impl = quote! {
        ::opslang_visitor::impl_visitor!(#ref_t_impl_generics #impl_type #ty_generics [visit] &T #ref_t_where_clause);
    };

    // Add tuple implementations
    // For 2-tuple (T1, T2)
    let tuple2_generics = update_generics(updated_generics, |g| {
        g.params.push(syn::parse_quote!(T1));
        g.params.push(syn::parse_quote!(T2));
        let tuple2_where_clause = g.make_where_clause();
        tuple2_where_clause
            .predicates
            .push(syn::parse_quote!(Self: ::opslang_visitor::Visitor<T1>));
        tuple2_where_clause
            .predicates
            .push(syn::parse_quote!(Self: ::opslang_visitor::Visitor<T2>));
    });

    let (tuple2_impl_generics, _, tuple2_where_clause) = tuple2_generics.split_for_impl();

    let tuple2_impl = quote! {
        ::opslang_visitor::impl_visitor!(#tuple2_impl_generics #impl_type #ty_generics [visit] (T1, T2) #tuple2_where_clause);
    };

    quote! {
        #str_impl
        #generic_impls
        #ref_impl
        #tuple2_impl
    }
}
