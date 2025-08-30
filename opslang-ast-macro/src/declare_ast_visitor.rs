use quote::{format_ident, quote};
use syn::{Token, braced, token};

use crate::ast_types::MethodKind;

pub struct TraitDeclaration {
    attrs: Vec<syn::Attribute>,
    vis: syn::Visibility,
    _trait_token: Token![trait],
    ident: syn::Ident,
    generics: syn::Generics,
    _brace_token: token::Brace,
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

/// Generates a comprehensive visitor trait for all V1 AST types.
pub fn declare_ast_visitor_trait(
    trait_decl: TraitDeclaration,
) -> syn::Result<proc_macro2::TokenStream> {
    let attrs = &trait_decl.attrs;
    let vis = &trait_decl.vis;
    let name = &trait_decl.ident;
    let ty_generics = &trait_decl.generics;
    let all_ast_types = crate::ast_types::AstType::get_v1_ast_node_types();

    // Generate visit_* and super_* method signatures
    let (visit_signatures, visit_methods): (Vec<_>, Vec<_>) = all_ast_types.iter().map(|ast_type| {
        let visit_name = ast_type.generate_visit_method_name(MethodKind::Visit);
        let super_name = ast_type.generate_visit_method_name(MethodKind::Super);
        let method_ident = format_ident!("{visit_name}");
        let super_method_ident = format_ident!("{super_name}");
        let super_qualified = ast_type.inside_of_v1_child_mod();
        let type_path = super_qualified.super_path();

        let sig = quote! {
            #[doc = "This method can be overridden by [`opslang_ast_macro::v1_ast_visitor_impl!`]."]
            fn #method_ident(&mut self, node: &#type_path);
            #[doc = "This method cannot be overridden."]
            fn #super_method_ident(&mut self, node: &#type_path);
        };
        let method = quote! {
            fn #method_ident(&mut self, node: &#type_path) {
                ::opslang_visitor::Visitor::<#type_path>::visit(self, node)
            }
            fn #super_method_ident(&mut self, node: &#type_path) {
                <#type_path as ::opslang_visitor::TemplateVisit<V>>::super_visit(node, self)
            }
        };
        (sig, method)
    }).unzip();

    // Generate trait bounds for Self: Visitor<Type1> + Visitor<Type2> + ...
    let visitor_bounds = all_ast_types.iter().map(|ast_type| {
        let super_qualified = ast_type.inside_of_v1_child_mod();
        let type_path = super_qualified.super_path();
        quote! {
            ::opslang_visitor::Visitor<#type_path>
        }
    });
    let ast_type_bounds = all_ast_types.iter().map(|ast_type| {
        let super_qualified = ast_type.inside_of_v1_child_mod();
        let type_path = super_qualified.super_path();
        quote! {
            #type_path: ::opslang_visitor::TemplateVisit<V>
        }
    });

    Ok(quote! {
        #(#attrs)*
        #vis trait #name #ty_generics {
            #(#visit_signatures)*
        }
        impl<'cx, V> #name #ty_generics for V
        where
            V: ?Sized #(+ #visitor_bounds)*,
            #(#ast_type_bounds),*
        {
            #(#visit_methods)*
        }
    })
}
