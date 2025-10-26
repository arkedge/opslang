use super::*;

/// Generates visitor implementation for AST types.
pub fn generate_visitor_impl(
    visitor_impl: VisitorImpl,
    callsite_trait: CallsiteTraitName,
    types: Vec<VisitorType>,
) -> syn::Result<proc_macro2::TokenStream> {
    let impl_type = &visitor_impl.impl_type;
    let impl_generics = &visitor_impl.impl_generics;
    let user_methods = &visitor_impl.methods;

    // Collect user-defined methods
    let mut user_method_map: std::collections::HashMap<String, &VisitorMethod> =
        user_methods.iter().map(|m| (m.name.clone(), m)).collect();

    // Generate Visitor implementations for each AST type
    let mut visitor_impls = Vec::new();
    let mut links_for_callsite = Vec::new();
    for visitor_type in &types {
        visitor_impls.push(generate_single_visitor_impl(
            visitor_type,
            impl_generics,
            impl_type,
            &callsite_trait,
            &mut user_method_map,
            &mut links_for_callsite,
        )?);
    }

    // Check for unused user methods and report errors
    if !user_method_map.is_empty() {
        let mut errors: VecDeque<syn::Error> = user_method_map
            .into_iter()
            .map(|(unused_name, unused_method)| {
                syn::Error::new(
                    unused_method.name_span,
                    format!(
                        "invalid visitor method `{unused_name}`\nfound no matching type for hook"
                    ),
                )
            })
            .collect();

        // Combine all errors into a single error
        let mut combined_error = errors.pop_front().unwrap();
        for error in errors {
            combined_error.combine(error);
        }
        return Err(combined_error);
    }

    let generics = syn::parse_quote!(<'cx>);
    let combined_generics = concatenate_generics(impl_generics, &generics);
    let (combined_impl_generics, _, combined_where_clause) = combined_generics.split_for_impl();

    Ok(quote! {
        #(#visitor_impls)*

        #[doc(hidden)]
        const _: () = {
            fn links_for_callsite #combined_impl_generics () #combined_where_clause {
                #(#links_for_callsite)*
            }
        };
    })
}

/// Holds the trait paths for callsite linking.
///
/// This information is not necessary for the visitor generation itself,
/// but is used to create references to the visitor methods to ensure
/// they are linked correctly at the callsite.
pub struct CallsiteTraitName {
    visitor: Path,
    visitor_mut: Option<Path>,
}

impl CallsiteTraitName {
    pub fn both(visitor: Path, visitor_mut: Path) -> Self {
        Self {
            visitor,
            visitor_mut: Some(visitor_mut),
        }
    }
    pub fn visitor_only(visitor: Path) -> Self {
        Self {
            visitor,
            visitor_mut: None,
        }
    }
}

/// Generate a single visitor implementation for a specific AST type.
fn generate_single_visitor_impl(
    visitor_type: &VisitorType,
    impl_generics: &Generics,
    impl_type: &Type,
    callsite_trait: &CallsiteTraitName,
    user_method_map: &mut std::collections::HashMap<String, &VisitorMethod>,
    links_for_callsite: &mut Vec<proc_macro2::TokenStream>,
) -> syn::Result<proc_macro2::TokenStream> {
    let VisitorType {
        generics,
        path,
        visit_method_name,
        mode,
    } = visitor_type;

    let trait_name = mode.trait_name();
    let method_name_str = mode.method_name();
    let method_name_ident = syn::Ident::new(method_name_str, proc_macro2::Span::call_site());

    let visit_fn = if let Some(user_method) = user_method_map.remove(visit_method_name) {
        // Found a user-defined method for this type. Use it and remove from map.
        let attrs = &user_method.attrs;
        let PatType { pat, ty, .. } = &user_method.param;
        let block = &user_method.block;
        let visit_method_name = syn::Ident::new(visit_method_name, user_method.name_span);
        let callsite_trait = match mode {
            VisitorMode::Visit => &callsite_trait.visitor,
            VisitorMode::VisitMut => callsite_trait.visitor_mut.as_ref().ok_or_else(|| {
                syn::Error::new(
                    user_method.name_span,
                    format!("no mutable visitor trait provided for method `{visit_method_name}`",),
                )
            })?,
        };
        links_for_callsite.push(quote! {
            let _ = <#impl_type as #callsite_trait>::#visit_method_name;
        });

        // Use user-defined method with their exact parameter and type
        quote! {
            #(#attrs)*
            fn #method_name_ident(&mut self, #pat: #ty) {
                #block
            }
        }
    } else {
        match mode {
            VisitorMode::Visit => quote! {
                #[inline]
                fn #method_name_ident(&mut self, node: &#path) {
                    <#path as ::opslang_visitor::TemplateVisit<Self>>::super_visit(node, self);
                }
            },
            VisitorMode::VisitMut => quote! {
                #[inline]
                fn #method_name_ident(&mut self, node: &mut #path) {
                    <#path as ::opslang_visitor::TemplateVisitMut<Self>>::super_visit_mut(node, self);
                }
            },
        }
    };

    // Concatenate generics properly
    let combined_generics = concatenate_generics(impl_generics, generics);
    let (combined_impl_generics, _, combined_where_clause) = combined_generics.split_for_impl();

    Ok(quote! {
        impl #combined_impl_generics #trait_name<#path> for #impl_type #combined_where_clause {
            #visit_fn
        }
    })
}
