use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    Data, DeriveInput, Fields, Ident, Token, WherePredicate, punctuated::Punctuated,
    visit_mut::VisitMut,
};

struct TypeParamReplacer<'a> {
    map: &'a HashMap<Ident, Ident>,
}

impl VisitMut for TypeParamReplacer<'_> {
    fn visit_type_path_mut(&mut self, node: &mut syn::TypePath) {
        // Continue visiting children first
        syn::visit_mut::visit_type_path_mut(self, node);

        // Then check if this path matches our target
        if let Some(first_segment) = node.path.segments.first_mut()
            && let Some(target) = self.map.get(&first_segment.ident)
        {
            let span = first_segment.ident.span();
            first_segment.ident.clone_from(target);
            first_segment.ident.set_span(span);
        }
    }

    fn visit_lifetime_mut(&mut self, node: &mut syn::Lifetime) {
        if let Some(target) = self.map.get(&node.ident) {
            let span = node.ident.span();
            node.ident.clone_from(target);
            node.ident.set_span(span);
        }
    }
}

pub fn derive_map_into_token(input: proc_macro::TokenStream) -> Result<TokenStream, TokenStream> {
    let input: DeriveInput = syn::parse(input).map_err(syn::Error::into_compile_error)?;

    let name = &input.ident;
    let (_impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    // Parse the enum data
    // In next refactoring, it is better to use `synstructure` to handle aggregation
    let data: Vec<_> = match &input.data {
        Data::Struct(data) => vec![(name, &data.fields)],
        Data::Enum(data) => data
            .variants
            .iter()
            .map(|v| (&v.ident, &v.fields))
            .collect(),
        _ => {
            return Err(syn::Error::new_spanned(
                input,
                "MapIntoToken can only be derived for enums",
            )
            .into_compile_error());
        }
    };

    // Generate match arms for each variant
    let mut match_arms = Vec::new();

    for (variant_name, fields) in &data {
        match fields {
            Fields::Unit => {
                match_arms.push(quote! {
                    #name::#variant_name => #name::#variant_name,
                });
            }
            Fields::Unnamed(fields) => {
                if fields.unnamed.len() == 1 {
                    match_arms.push(quote! {
                        #name::#variant_name(field) => #name::#variant_name(field.into_token()),
                    });
                } else {
                    return Err(syn::Error::new_spanned(
                        variant_name,
                        "MapIntoToken only supports variants with exactly one field",
                    )
                    .into_compile_error());
                }
            }
            Fields::Named(fields) => {
                let (bindings, rhs): (Punctuated<_, Token![,]>, Punctuated<_, Token![,]>) = fields
                    .named
                    .iter()
                    .map(|f| {
                        let id = f.ident.as_ref().unwrap();
                        (id, quote! { #id: #id.into_token() })
                    })
                    .unzip();
                match_arms.push(quote! {
                    #name {
                        #bindings
                    } => #name {
                        #rhs
                    },
                });
            }
        }
    }

    // Create new generics with renamed type parameters
    let mut new_generics = input.generics.clone();
    let mut param_map = std::collections::HashMap::new();

    // Rename all parameters by appending suffixes
    for param in &mut new_generics.params {
        match param {
            syn::GenericParam::Type(type_param) => {
                let old_name = type_param.ident.clone();
                let new_name = format!("{old_name}Target");
                let new_ident = syn::Ident::new(&new_name, type_param.ident.span());
                param_map.insert(old_name, new_ident.clone());
                type_param.ident = new_ident;
            }
            syn::GenericParam::Lifetime(lifetime_param) => {
                let old_name = lifetime_param.lifetime.ident.clone();
                let new_name = format!("{old_name}_target");
                let new_ident = syn::Ident::new(&new_name, lifetime_param.lifetime.ident.span());
                param_map.insert(old_name, new_ident.clone());
                lifetime_param.lifetime.ident = new_ident;
            }
            syn::GenericParam::Const(_) => {} // Don't handle const generics for now
        }
    }

    let mut replacer = TypeParamReplacer { map: &param_map };

    // Also rename references in bounds and where clauses
    replacer.visit_generics_mut(&mut new_generics);

    // Get new_ty_generics before moving new_generics
    let new_generics_for_ty = new_generics.clone();
    let (_, new_ty_generics, _) = new_generics_for_ty.split_for_impl();

    // Build where clause predicates
    let mut predicates = Punctuated::<WherePredicate, Token![,]>::new();
    if let Some(where_clause) = where_clause {
        predicates.clone_from(&where_clause.predicates);
    }

    // Add constraints for each variant field that needs IntoToken
    for (_, variant) in &data {
        match variant {
            Fields::Unnamed(fields) if fields.unnamed.len() == 1 => {
                let field_type = &fields.unnamed.first().unwrap().ty;
                // Create target type by replacing type parameters with renamed ones
                let mut target_type = field_type.clone();
                replacer.visit_type_mut(&mut target_type);

                predicates.push(syn::parse_quote! {
                    #field_type: crate::syntax::v1::token::IntoToken<#target_type>
                });
            }
            Fields::Named(fields) => {
                for field_type in fields.named.iter().map(|f| &f.ty) {
                    // Create target type by replacing type parameters with renamed ones
                    let mut target_type = field_type.clone();

                    replacer.visit_type_mut(&mut target_type);

                    predicates.push(syn::parse_quote! {
                        #field_type: crate::syntax::v1::token::IntoToken<#target_type>
                    });
                }
            }
            _ => unreachable!(),
        }
    }

    // Merge impl_generics and new_impl_generics
    let mut merged_generics = input.generics.clone();
    for param in new_generics.params {
        merged_generics.params.push(param);
    }
    let (merged_impl_generics, _, _) = merged_generics.split_for_impl();

    let expanded = quote! {
        impl #merged_impl_generics crate::syntax::v1::token::IntoToken<#name #new_ty_generics> for #name #ty_generics
        where
            #predicates
        {
            fn into_token(self) -> #name #new_ty_generics {
                match self {
                    #(#match_arms)*
                }
            }
        }
    };

    Ok(expanded)
}
