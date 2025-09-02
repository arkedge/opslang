use quote::{format_ident, quote};
use syn::{Token, braced, token};

/// Trait declaration structure that can be parsed from input.
pub struct TraitDeclaration {
    pub attrs: Vec<syn::Attribute>,
    pub vis: syn::Visibility,
    pub _trait_token: Token![trait],
    pub ident: syn::Ident,
    pub generics: syn::Generics,
    pub _brace_token: token::Brace,
}

impl syn::parse::Parse for TraitDeclaration {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let _content;
        Ok(Self {
            attrs: input.call(syn::Attribute::parse_outer)?,
            vis: input.parse()?,
            _trait_token: input.parse()?,
            ident: input.parse()?,
            generics: input.parse()?,
            _brace_token: braced!(_content in input),
        })
    }
}

/// Type interface for visitor trait generation.
pub trait VisitableType {
    /// Generate the method name for this type based on method kind and visitor mode.
    fn generate_visit_method_name(
        &self,
        kind: crate::MethodKind,
        mode: crate::VisitorMode,
    ) -> String;

    /// Generate the full type path for this type.
    fn full_type_path(&self) -> proc_macro2::TokenStream;
}

/// Generate visitor trait implementation.
pub fn declare_visitor_trait<T: VisitableType>(
    trait_decl: TraitDeclaration,
    types: impl Iterator<Item = T>,
    mode: crate::VisitorMode,
) -> syn::Result<proc_macro2::TokenStream> {
    let TraitDeclaration {
        attrs,
        vis,
        ident,
        generics,
        ..
    } = &trait_decl;
    let types: Vec<_> = types.into_iter().collect();

    // Generate visit_* and super_* method signatures
    let (visit_signatures, visit_methods): (Vec<_>, Vec<_>) = types.iter()
        .map(|visitor_type| {
            let visit_name =
                visitor_type.generate_visit_method_name(crate::MethodKind::Visit, mode);
            let super_name =
                visitor_type.generate_visit_method_name(crate::MethodKind::Super, mode);
            let method_ident = format_ident!("{visit_name}");
            let super_method_ident = format_ident!("{super_name}");
            let type_path = visitor_type.full_type_path();

            let param_type = match mode {
                crate::VisitorMode::Visit => quote! { &#type_path },
                crate::VisitorMode::VisitMut => quote! { &mut #type_path },
            };

            let sig = quote! {
                #[doc = "This method can be overridden by visitor implementation macros."]
                fn #method_ident(&mut self, node: #param_type);
                #[doc = "This method cannot be overridden."]
                fn #super_method_ident(&mut self, node: #param_type);
            };
            let method = match mode {
                crate::VisitorMode::Visit => quote! {
                    fn #method_ident(&mut self, node: #param_type) {
                        ::opslang_visitor::Visitor::<#type_path>::visit(self, node)
                    }
                    fn #super_method_ident(&mut self, node: #param_type) {
                        <#type_path as ::opslang_visitor::TemplateVisit<V>>::super_visit(node, self)
                    }
                },
                crate::VisitorMode::VisitMut => quote! {
                    fn #method_ident(&mut self, node: #param_type) {
                        ::opslang_visitor::VisitorMut::<#type_path>::visit_mut(self, node)
                    }
                    fn #super_method_ident(&mut self, node: #param_type) {
                        <#type_path as ::opslang_visitor::TemplateVisitMut<V>>::super_visit_mut(node, self)
                    }
                },
            };
            (sig, method)
        })
        .unzip();

    // Generate trait bounds for Self: Visitor<Type1> + Visitor<Type2> + ...
    let (visitor_bounds, node_type_bounds) = match mode {
        crate::VisitorMode::Visit => {
            let visitor_bounds = types.iter().map(|visitor_type| {
                let type_path = visitor_type.full_type_path();
                quote! {
                    ::opslang_visitor::Visitor<#type_path>
                }
            });
            let ast_type_bounds = types.iter().map(|visitor_type| {
                let type_path = visitor_type.full_type_path();
                quote! {
                    #type_path: ::opslang_visitor::TemplateVisit<V>
                }
            });
            (
                visitor_bounds.collect::<Vec<_>>(),
                ast_type_bounds.collect::<Vec<_>>(),
            )
        }
        crate::VisitorMode::VisitMut => {
            let visitor_bounds = types.iter().map(|visitor_type| {
                let type_path = visitor_type.full_type_path();
                quote! {
                    ::opslang_visitor::VisitorMut<#type_path>
                }
            });
            let node_type_bounds = types.iter().map(|visitor_type| {
                let type_path = visitor_type.full_type_path();
                quote! {
                    #type_path: ::opslang_visitor::TemplateVisitMut<V>
                }
            });
            (
                visitor_bounds.collect::<Vec<_>>(),
                node_type_bounds.collect::<Vec<_>>(),
            )
        }
    };

    Ok(quote! {
        #(#attrs)*
        #vis trait #ident #generics {
            #(#visit_signatures)*
        }
        impl<'cx, V> #ident #generics for V
        where
            V: ?Sized #(+ #visitor_bounds)*,
            #(#node_type_bounds),*
        {
            #(#visit_methods)*
        }
    })
}
