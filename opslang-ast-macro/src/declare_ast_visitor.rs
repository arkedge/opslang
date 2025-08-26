use convert_case::{Case, Casing};
use quote::{format_ident, quote};
use syn::{Token, braced, token};

struct TraitDeclaration {
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
    input: proc_macro::TokenStream,
) -> Result<proc_macro2::TokenStream, proc_macro2::TokenStream> {
    let trait_decl: TraitDeclaration = syn::parse(input).map_err(|e| e.to_compile_error())?;

    let attrs = &trait_decl.attrs;
    let vis = &trait_decl.vis;
    let name = &trait_decl.ident;
    let ty_generics = &trait_decl.generics;
    let all_ast_types = crate::ast_types::AstType::get_v1_ast_types();

    // Generate visit_* and super_* method signatures
    let visit_methods = all_ast_types.iter().map(|ast_type| {
        let safe_name = ast_type.ident_safe_name();
        let method_ident = format_ident!("visit_{}", safe_name.to_case(Case::Snake));
        let super_method_ident = format_ident!("super_{}", safe_name.to_case(Case::Snake));
        let type_path = ast_type.type_path();

        quote! {
            fn #method_ident(&mut self, node: &#type_path) {
                ::opslang_visitor::Visitor::<#type_path>::visit(self, node)
            }

            fn #super_method_ident(&mut self, node: &#type_path) {
                <#type_path as ::opslang_visitor::TemplateVisit<V>>::super_visit(node, self)
            }
        }
    });

    let visit_signatures = all_ast_types.iter().map(|ast_type| {
        let safe_name = ast_type.ident_safe_name();
        let method_ident = format_ident!("visit_{}", safe_name.to_case(Case::Snake));
        let super_method_ident = format_ident!("super_{}", safe_name.to_case(Case::Snake));
        let type_path = ast_type.type_path();

        quote! {
            fn #method_ident(&mut self, node: &#type_path);
            fn #super_method_ident(&mut self, node: &#type_path);
        }
    });

    // Generate trait bounds for Self: Visitor<Type1> + Visitor<Type2> + ...
    let visitor_bounds = all_ast_types.iter().map(|ast_type| {
        let type_path = ast_type.type_path();
        quote! {
            ::opslang_visitor::Visitor<#type_path>
        }
    });
    let ast_type_bounds = all_ast_types.iter().map(|ast_type| {
        let type_path = ast_type.type_path();
        quote! {
            #type_path: ::opslang_visitor::TemplateVisit<V>
        }
    });

    Ok(quote! {
        #(#attrs)*
        #vis trait #name #ty_generics {
            #(#visit_signatures)*
        }
        impl<'cx, V: ?Sized #(+ #visitor_bounds)*> #name #ty_generics for V
        where
            #(#ast_type_bounds),*
        {
            #(#visit_methods)*
        }
    })
}
