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

/// Visitor implementation mode for generic types.
#[derive(Debug, Clone, Copy)]
pub enum VisitorMode {
    /// Generate immutable visitor implementations using `[visit]`
    Visit,
    /// Generate mutable visitor implementations using `[visit_mut]`
    VisitMut,
}

impl VisitorMode {
    /// Returns the visitor mode as a token stream for use in `impl_visitor!` macro.
    fn as_tokens(&self) -> proc_macro2::TokenStream {
        use quote::quote;
        match self {
            VisitorMode::Visit => quote!([visit]),
            VisitorMode::VisitMut => quote!([visit_mut]),
        }
    }

    /// Returns the trait name for constraints.
    fn trait_name(&self) -> proc_macro2::TokenStream {
        use quote::quote;
        match self {
            VisitorMode::Visit => quote!(::opslang_visitor::Visitor),
            VisitorMode::VisitMut => quote!(::opslang_visitor::VisitorMut),
        }
    }
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
/// * `mode`:
///   The visitor mode (Visit or VisitMut) determining which trait implementations to generate.
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
/// Panics if `updated_generics.params.is_empty()`, indicating the generics don't have parameters,
/// which disrupts `::opslang_visitor::impl_visitor!` call.
fn generate_generic_visitor_impls_internal(
    ty_generics: &syn::TypeGenerics,
    updated_generics: &syn::Generics,
    impl_type: &syn::Type,
    mode: VisitorMode,
) -> proc_macro2::TokenStream {
    use quote::quote;

    let t = quote!(#impl_type #ty_generics);
    let mode_tokens = mode.as_tokens();
    let trait_name = mode.trait_name();

    assert!(!updated_generics.params.is_empty());
    let (impl_generics, _, where_clause) = updated_generics.split_for_impl();

    // Base implementation for str (no additional generics)
    let str_impl = quote! {
        ::opslang_visitor::impl_visitor!(#impl_generics #t #mode_tokens str #where_clause);
    };

    let t_generics = update_generics(updated_generics, |g| {
        // Add T to generics for Option<T>, [T] and &T
        g.params.push(syn::parse_quote!(T));

        // Add where clause for T
        g.make_where_clause()
            .predicates
            .push(syn::parse_quote!(Self: #trait_name<T>))
    });

    let (t_impl_generics, _, t_where_clause) = t_generics.split_for_impl();

    let generic_impls = quote! {
        ::opslang_visitor::impl_visitor!(#t_impl_generics #t #mode_tokens Option<T> #t_where_clause);
        ::opslang_visitor::impl_visitor!(#t_impl_generics #t #mode_tokens [T] #t_where_clause);
    };

    let ref_t_generics = match mode {
        VisitorMode::Visit => {
            update_generics(&t_generics, |g| {
                // For &T, we need T: ?Sized to handle &str, &[U], etc.
                g.make_where_clause()
                    .predicates
                    .push(syn::parse_quote!(T: ?Sized))
            })
        }
        VisitorMode::VisitMut => {
            update_generics(updated_generics, |g| {
                // Add T to generics for &mut T
                g.params.push(syn::parse_quote!(T));

                // Add where clause for T
                let trait_name = VisitorMode::Visit.trait_name();
                g.make_where_clause()
                    .predicates
                    .push(syn::parse_quote!(Self: #trait_name<T>));

                // For &mut T, we need T: ?Sized to handle &mut str, &mut [U], etc.
                g.make_where_clause()
                    .predicates
                    .push(syn::parse_quote!(T: ?Sized))
            })
        }
    };

    let (ref_t_impl_generics, _, ref_t_where_clause) = ref_t_generics.split_for_impl();

    let ref_impl = quote! {
        ::opslang_visitor::impl_visitor!(#ref_t_impl_generics #t #mode_tokens &T #ref_t_where_clause);
    };

    let ref_mut_t_generics = update_generics(&t_generics, |g| {
        // For &mut T, we need T: ?Sized to handle &mut str, &mut [U], etc.
        g.make_where_clause()
            .predicates
            .push(syn::parse_quote!(T: ?Sized))
    });

    let (ref_mut_t_impl_generics, _, ref_mut_t_where_clause) = ref_mut_t_generics.split_for_impl();

    let ref_mut_impl = quote! {
        ::opslang_visitor::impl_visitor!(#ref_mut_t_impl_generics #t #mode_tokens &mut T #ref_mut_t_where_clause);
    };

    // Add tuple implementations
    let tuple2_generics = update_generics(updated_generics, |g| {
        // For 2-tuple (T1, T2)
        g.params.push(syn::parse_quote!(T1));
        g.params.push(syn::parse_quote!(T2));

        let tuple2_where_clause = g.make_where_clause();
        tuple2_where_clause
            .predicates
            .push(syn::parse_quote!(Self: #trait_name<T1>));
        tuple2_where_clause
            .predicates
            .push(syn::parse_quote!(Self: #trait_name<T2>));
    });

    let (tuple2_impl_generics, _, tuple2_where_clause) = tuple2_generics.split_for_impl();

    let tuple2_impl = quote! {
        ::opslang_visitor::impl_visitor!(#tuple2_impl_generics #t #mode_tokens (T1, T2) #tuple2_where_clause);
    };

    quote! {
        #str_impl
        #generic_impls
        #ref_impl
        #ref_mut_impl
        #tuple2_impl
    }
}

/// Generate generic visitor implementations for immutable visiting.
pub fn generate_generic_visitor_impls(
    ty_generics: &syn::TypeGenerics,
    updated_generics: &syn::Generics,
    impl_type: &syn::Type,
) -> proc_macro2::TokenStream {
    generate_generic_visitor_impls_internal(
        ty_generics,
        updated_generics,
        impl_type,
        VisitorMode::Visit,
    )
}

/// Generate generic visitor implementations for mutable visiting.
pub fn generate_generic_visitor_mut_impls(
    ty_generics: &syn::TypeGenerics,
    updated_generics: &syn::Generics,
    impl_type: &syn::Type,
) -> proc_macro2::TokenStream {
    generate_generic_visitor_impls_internal(
        ty_generics,
        updated_generics,
        impl_type,
        VisitorMode::VisitMut,
    )
}
