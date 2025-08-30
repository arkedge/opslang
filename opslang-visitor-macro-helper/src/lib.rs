use std::{borrow::Cow, collections::VecDeque};

use quote::quote;
use syn::{
    Attribute, Block, FnArg, Generics, ItemFn, PatType, Path, ReturnType, Token, Type,
    parse::{Parse, ParseStream},
};

pub mod no_intermediate_helper;
pub mod shared_visitor_trait;

/// Method kind for generating method names.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MethodKind {
    /// Generate `visit_*` method names
    Visit,
    /// Generate `super_*` method names (for identifier-safe names)
    Super,
}

/// Validated visitor method with checked parameter and block.
pub struct VisitorMethod {
    pub attrs: Vec<Attribute>,
    pub name: String,
    pub param: PatType,
    pub block: Block,

    /// For diagnostic purpose
    pub name_span: proc_macro2::Span,
}

/// Structure representing a visitor implementation block.
pub struct VisitorImpl {
    pub impl_generics: Generics,
    pub impl_type: Type,
    pub methods: Vec<VisitorMethod>,
}

impl Parse for VisitorImpl {
    fn parse(input: ParseStream) -> syn::Result<Self> {
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
            let item_fn = content.parse::<ItemFn>()?;
            let validated_method = validate_visitor_method(item_fn)?;
            methods.push(validated_method);
        }

        Ok(VisitorImpl {
            impl_generics,
            impl_type,
            methods,
        })
    }
}

/// Validates a visitor method function to ensure it has the correct signature.
fn validate_visitor_method(item_fn: ItemFn) -> syn::Result<VisitorMethod> {
    let sig = &item_fn.sig;

    // Check for invalid modifiers
    if sig.constness.is_some() {
        return Err(syn::Error::new_spanned(
            sig.constness,
            "visitor methods cannot be const",
        ));
    }
    if sig.asyncness.is_some() {
        return Err(syn::Error::new_spanned(
            sig.asyncness,
            "visitor methods cannot be async",
        ));
    }
    if sig.unsafety.is_some() {
        return Err(syn::Error::new_spanned(
            sig.unsafety,
            "visitor methods cannot be unsafe",
        ));
    }
    if sig.abi.is_some() {
        return Err(syn::Error::new_spanned(
            &sig.abi,
            "visitor methods cannot have custom ABI",
        ));
    }

    // Check return type (should be () or omitted)
    match &sig.output {
        ReturnType::Default => {}
        ReturnType::Type(_, ty) => {
            if let Type::Tuple(tuple) = ty.as_ref() {
                if !tuple.elems.is_empty() {
                    return Err(syn::Error::new_spanned(
                        ty,
                        "visitor methods must return () or have no return type",
                    ));
                }
            } else {
                return Err(syn::Error::new_spanned(
                    ty,
                    "visitor methods must return () or have no return type",
                ));
            }
        }
    }

    // Check that signature has exactly 2 parameters: &mut self and one other parameter
    if sig.inputs.len() != 2 {
        return Err(syn::Error::new_spanned(
            &sig.inputs,
            "visitor methods must have exactly 2 parameters: &mut self and the node parameter",
        ));
    }

    // Check first parameter is &mut self
    if let Some(FnArg::Receiver(receiver)) = sig.inputs.first() {
        if receiver.reference.is_none() || receiver.mutability.is_none() {
            return Err(syn::Error::new_spanned(
                receiver,
                "first parameter must be &mut self",
            ));
        }
    } else {
        return Err(syn::Error::new_spanned(
            sig.inputs.first().unwrap(),
            "first parameter must be &mut self",
        ));
    }

    let sig = item_fn.sig;

    // The second parameter should be a reference, we'll validate the exact type later
    let arg = sig.inputs.into_iter().nth(1).unwrap();
    let param = if let FnArg::Typed(param) = arg {
        // Valid - we have a typed parameter
        param
    } else {
        return Err(syn::Error::new_spanned(
            arg,
            "second parameter must be a typed parameter",
        ));
    };

    Ok(VisitorMethod {
        attrs: item_fn.attrs,
        name: sig.ident.to_string(),
        name_span: sig.ident.span(),
        param,
        block: *item_fn.block,
    })
}

/// Visitor mode indicating whether to generate immutable or mutable visitor implementations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VisitorMode {
    /// Generate immutable visitor implementations (`Visitor` trait, `visit` method)
    Visit,
    /// Generate mutable visitor implementations (`VisitorMut` trait, `visit_mut` method)  
    VisitMut,
}

impl VisitorMode {
    /// Returns the trait name for this visitor mode.
    pub fn trait_name(&self) -> proc_macro2::TokenStream {
        use quote::quote;
        match self {
            VisitorMode::Visit => quote!(::opslang_visitor::Visitor),
            VisitorMode::VisitMut => quote!(::opslang_visitor::VisitorMut),
        }
    }

    /// Returns the method name for this visitor mode.
    pub fn method_name(&self) -> &'static str {
        match self {
            VisitorMode::Visit => "visit",
            VisitorMode::VisitMut => "visit_mut",
        }
    }
}

/// Type definition for visitor implementation generation.
pub struct VisitorType {
    pub generics: Generics,
    pub path: Path,
    pub visit_method_name: String,
    pub mode: VisitorMode,
}

/// Check if generics are empty (equivalent to default)
fn is_empty_generics(generics: &Generics) -> bool {
    generics.params.is_empty() && generics.where_clause.is_none()
}

/// Concatenates two Generics, combining parameters and where clauses.
fn concatenate_generics<'a>(left: &'a Generics, right: &'a Generics) -> Cow<'a, Generics> {
    if is_empty_generics(left) {
        return Cow::Borrowed(right);
    } else if is_empty_generics(right) {
        return Cow::Borrowed(left);
    }
    let mut combined = left.clone();

    // Extend parameters
    combined.params.extend(right.params.iter().cloned());

    // Ensure we have angle bracket tokens if we have parameters
    if !combined.params.is_empty() && combined.lt_token.is_none() {
        combined.lt_token = Some(left.lt_token.or(right.lt_token).unwrap());
        combined.gt_token = Some(left.gt_token.or(right.gt_token).unwrap());
    }

    // Combine where clauses
    if let Some(type_where) = &right.where_clause {
        if combined.where_clause.is_some() {
            combined
                .make_where_clause()
                .predicates
                .extend(type_where.predicates.iter().cloned());
        } else {
            *combined.make_where_clause() = type_where.clone();
        }
    }

    Cow::Owned(combined)
}

/// Generates visitor implementation for AST types.
pub fn generate_visitor_impl(
    visitor_impl: VisitorImpl,
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
    for visitor_type in &types {
        visitor_impls.push(generate_single_visitor_impl(
            visitor_type,
            impl_generics,
            impl_type,
            &mut user_method_map,
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

    Ok(quote! {
        #(#visitor_impls)*
    })
}

/// Generate a single visitor implementation for a specific AST type.
fn generate_single_visitor_impl(
    visitor_type: &VisitorType,
    impl_generics: &Generics,
    impl_type: &Type,
    user_method_map: &mut std::collections::HashMap<String, &VisitorMethod>,
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
        let attrs = &user_method.attrs;
        let PatType { pat, ty, .. } = &user_method.param;
        let block = &user_method.block;

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

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    #[test]
    fn test_visitor_impl_parse() {
        // Test simple case without generics
        let input = parse_quote! {
            for TestVisitor {
                fn visit_test(&mut self, node: &TestNode) {
                    println!("test");
                }
            }
        };
        let parsed: VisitorImpl = input;
        assert_eq!(parsed.methods.len(), 1);

        // Test with generics
        let input2 = parse_quote! {
            <T> for T {
                fn visit_generic(&mut self, item: &GenericNode) {
                    println!("generic");
                }
            }
        };
        let parsed2: VisitorImpl = input2;
        assert_eq!(parsed2.methods.len(), 1);
    }

    #[test]
    fn test_visitor_method_validation() {
        use syn::parse_quote;

        // Valid method
        let valid_fn: ItemFn = parse_quote! {
            fn visit_test(&mut self, node: &TestNode) {
                println!("test");
            }
        };
        assert!(validate_visitor_method(valid_fn).is_ok());

        // Invalid: const method
        let const_fn: ItemFn = parse_quote! {
            const fn visit_test(&mut self, node: &TestNode) {
                println!("test");
            }
        };
        assert!(validate_visitor_method(const_fn).is_err());

        // Invalid: async method
        let async_fn: ItemFn = parse_quote! {
            async fn visit_test(&mut self, node: &TestNode) {
                println!("test");
            }
        };
        assert!(validate_visitor_method(async_fn).is_err());

        // Invalid: wrong number of parameters
        let wrong_params: ItemFn = parse_quote! {
            fn visit_test(&mut self) {
                println!("test");
            }
        };
        assert!(validate_visitor_method(wrong_params).is_err());
    }
}
