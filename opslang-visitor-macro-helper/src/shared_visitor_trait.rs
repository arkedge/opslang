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
    // Expansion of `unzip` for 3-tuples is not supported in std.
    let mut unzipped: (Vec<_>, Vec<_>, Vec<_>) = Default::default();
    let it = types.iter()
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
            let doc_method = match mode {
                crate::VisitorMode::Visit => quote! {
                    fn #method_ident(&mut self, node: #param_type) {
                        unimplemented!()
                    }
                    fn #super_method_ident(&mut self, node: #param_type) {
                        unimplemented!()
                    }
                },
                crate::VisitorMode::VisitMut => quote! {
                    fn #method_ident(&mut self, node: #param_type) {
                        unimplemented!()
                    }
                    fn #super_method_ident(&mut self, node: #param_type) {
                        unimplemented!()
                    }
                },
            };
            (sig, method, doc_method)
        });
    unzipped.extend(it);
    let (visit_signatures, visit_methods, doc_visit_methods) = unzipped;

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
    let doc_only_trait_name = match mode {
        crate::VisitorMode::Visit => format_ident!("TooManyBoundsForDocumentation"),
        crate::VisitorMode::VisitMut => format_ident!("TooManyBoundsForDocumentationMut"),
    };
    let doc_hidden_bounds_len = visitor_bounds.len();
    let doc_crate_link = format!(
        "Actual trait definition is generated by macro. This documentation and the macro are located at `{}` crate.",
        env!("CARGO_CRATE_NAME")
    );

    Ok(quote! {
        #(#attrs)*
        #vis trait #ident #generics {
            #(#visit_signatures)*
        }
        #[cfg(not(doc))]
        impl<'cx, V> #ident #generics for V
        where
            V: ?Sized #(+ #visitor_bounds)*,
            #(#node_type_bounds),*
        {
            #(#visit_methods)*
        }

        // below is only for documentation purposes

        #[cfg(doc)]
        #[doc(hidden)]
        trait #doc_only_trait_name {}

        /// Implements the visitor trait for any type that visits all node types.
        ///
        /// This enables referring to the visitor methods by its name like
        /// `visitor.visit_ty` instead of `use`-ing the `Visitor` trait directly,
        /// which accepts all node types as parameters and may introduce bugs
        /// by accident.
        ///
        /// In fact, `V` needs to implement `Visitor<Type>` or `VisitorMut<Type>
        /// for each node type and all node types should be visitable.
        /// We omit this from the documentation to reduce clutter.
        #[doc = ""]
        #[doc = #doc_crate_link]
        #[doc = ""]
        #[doc = concat!("This impl has ", #doc_hidden_bounds_len, " * 2 bounds that are hidden for documentation purposes.")]
        #[cfg(doc)]
        impl<'cx, V> #ident #generics for V
        where
            V: ?Sized + #doc_only_trait_name,
        {
            #(#doc_visit_methods)*
        }
    })
}
