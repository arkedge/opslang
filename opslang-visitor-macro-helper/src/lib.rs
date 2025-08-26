use std::borrow::Cow;

use convert_case::{Case, Casing};
use quote::quote;
use syn::{
    Generics, ItemFn, Path, Token, Type,
    parse::{Parse, ParseStream},
};

/// Structure representing a visitor implementation block.
pub struct VisitorImpl {
    pub impl_generics: Generics,
    pub impl_type: Type,
    pub methods: Vec<ItemFn>,
}

impl Parse for VisitorImpl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse::<Token![impl]>()?;

        let mut impl_generics = input.parse::<Generics>()?;

        input.parse::<Token![for]>()?;
        let impl_type = input.parse::<Type>()?;

        // Parse where clause if present
        if input.peek(Token![where]) {
            impl_generics.where_clause = Some(input.parse()?);
        }

        let content;
        syn::braced!(content in input);

        let mut methods = Vec::new();
        while !content.is_empty() {
            methods.push(content.parse::<ItemFn>()?);
        }

        Ok(VisitorImpl {
            impl_generics,
            impl_type,
            methods,
        })
    }
}

/// Type definition for visitor implementation generation.
pub struct VisitorType {
    pub generics: Generics,
    pub path: Path,
}

/// Check if generics are empty (equivalent to default)
fn is_empty_generics(generics: &Generics) -> bool {
    generics.params.is_empty() && generics.where_clause.is_none()
}

/// Concatenates two Generics, combining parameters and where clauses.
fn concatenate_generics<'a>(
    impl_generics: &'a Generics,
    type_generics: &'a Generics,
) -> Cow<'a, Generics> {
    if is_empty_generics(impl_generics) {
        return Cow::Borrowed(type_generics);
    } else if is_empty_generics(type_generics) {
        return Cow::Borrowed(impl_generics);
    }
    let mut combined = impl_generics.clone();

    // Extend parameters
    combined.params.extend(type_generics.params.iter().cloned());

    // Ensure we have angle bracket tokens if we have parameters
    if !combined.params.is_empty() && combined.lt_token.is_none() {
        combined.lt_token = Some(impl_generics.lt_token.or(type_generics.lt_token).unwrap());
        combined.gt_token = Some(impl_generics.gt_token.or(type_generics.gt_token).unwrap());
    }

    // Combine where clauses
    match (&combined.where_clause, &type_generics.where_clause) {
        (Some(impl_where), Some(type_where)) => {
            let mut combined_where = impl_where.clone();
            combined_where
                .predicates
                .extend(type_where.predicates.iter().cloned());
            combined.where_clause = Some(combined_where);
        }
        (None, Some(type_where)) => {
            combined.where_clause = Some(type_where.clone());
        }
        _ => {} // Keep existing or none
    }

    Cow::Owned(combined)
}

/// Generates visitor implementation for AST types.
pub fn generate_visitor_impl(
    visitor_impl: VisitorImpl,
    types: Vec<VisitorType>,
) -> proc_macro2::TokenStream {
    let impl_type = &visitor_impl.impl_type;
    let impl_generics = &visitor_impl.impl_generics;
    let user_methods = &visitor_impl.methods;

    // Collect user-defined methods
    let user_method_map: std::collections::HashMap<String, &ItemFn> = user_methods
        .iter()
        .map(|m| (m.sig.ident.to_string(), m))
        .collect();

    // Generate Visitor implementations for each AST type
    let visitor_impls = types.iter().map(|visitor_type| {
        let VisitorType { generics, path } = visitor_type;
        let type_ident = &path.segments.last().unwrap().ident;
        let visit_method_name = format!("visit_{}", type_ident.to_string().to_case(Case::Snake));

        let visit_fn = if let Some(user_method) = user_method_map.get(&visit_method_name) {
            let block = &user_method.block;
            // Use user-defined method
            quote! {
                fn visit(&mut self, node: &#path) {
                    #block
                }
            }
        } else {
            quote! {
                fn visit(&mut self, node: &#path) {
                    <#path as ::opslang_visitor::TemplateVisit<Self>>::super_visit(node, self);
                }
            }
        };

        // Concatenate generics properly
        let combined_generics = concatenate_generics(impl_generics, generics);
        let (combined_impl_generics, _, combined_where_clause) = combined_generics.split_for_impl();

        quote! {
            impl #combined_impl_generics ::opslang_visitor::Visitor<#path> for #impl_type #combined_where_clause {
                #visit_fn
            }
        }
    });

    quote! {
        #(#visitor_impls)*
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    #[test]
    fn test_visitor_impl_parse() {
        // Test simple case without generics
        let input = parse_quote! {
            impl for TestVisitor {
                fn visit_test(&mut self) {
                    println!("test");
                }
            }
        };
        let parsed: VisitorImpl = input;
        assert_eq!(parsed.methods.len(), 1);

        // Test with generics
        let input2 = parse_quote! {
            impl<T> for T {
                fn visit_generic(&mut self) {
                    println!("generic");
                }
            }
        };
        let parsed2: VisitorImpl = input2;
        assert_eq!(parsed2.methods.len(), 1);
    }
}
