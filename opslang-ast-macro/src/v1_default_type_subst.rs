use proc_macro2::TokenStream;
use quote::quote;
use syn::{Ident, Token, Type, parse::Parse};

pub struct TypeSubstitution {
    overrides: Vec<TypeOverride>,
    #[allow(dead_code)]
    has_default: bool,
}

struct TypeOverride {
    name: Ident,
    ty: Type,
}

impl Parse for TypeSubstitution {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut overrides = Vec::new();
        let mut has_default = false;

        while !input.is_empty() {
            if input.peek(Token![..]) {
                input.parse::<Token![..]>()?;
                has_default = true;
                break;
            }

            let name: Ident = input.parse()?;
            input.parse::<Token![=]>()?;
            let ty: Type = input.parse()?;

            // Optional comma
            if input.peek(Token![,]) {
                input.parse::<Token![,]>()?;
            }

            overrides.push(TypeOverride { name, ty });
        }

        Ok(TypeSubstitution {
            overrides,
            has_default,
        })
    }
}

/// A macro to define default types in a more Rust-like syntax.
///
/// This macro accepts type definitions like `type Span = syntax::v1::Span;`
/// and converts them to a list of (name, path) tuples using `stringify!`.
/// Uses a dummy trait to ensure proper call_site for tokens.
macro_rules! define_default_types {
    ($(type $name:ident = $path:ty;)*) => {{
        {
            trait __DummyTypesForCallSite {
                $(type $name;)*
            }
            &[
                $((stringify!($name), stringify!($path)),)*
            ]
        }
    }};
}

/// Default type definitions for the v1 syntax.
///
/// This matches the structure of the original `v1_default_type_subst!` macro.
/// The order must match the order in the original macro.
const DEFAULT_TYPES: &[(&str, &str)] = define_default_types! {
    type Span = syn::Span;
    type Position = syn::Position;
    type Comment = &'cx syn::Comment<'cx, Self>;
    type Row = &'cx syn::Row<'cx, Self>;
    type Statement = syn::Statement<'cx, Self>;
    type Block = &'cx syn::Block<'cx, Self>;
    type ScopeItem = syn::ScopeItem<'cx, Self>;
    type ReturnStmt = syn::ReturnStmt<'cx, Self>;
    type Ident = syn::Ident<'cx, Self>;
    type Path = syn::Path<'cx, Self>;
    type Ty = syn::Path<'cx, Self>;
    type FnReturnTy = syn::FnReturnTy<'cx, Self>;
    type Expr = syn::Expr<'cx, Self>;
    type Qualif = syn::Qualif<'cx, Self>;
    type PreQualified = syn::PreQualified<'cx, Self>;
    type Parened = syn::Parened<'cx, Self>;
    type Literal = syn::Literal<'cx, Self>;
    type Array = syn::literal::Array<'cx, Self>;
    type String = syn::literal::String<'cx, Self>;
    type Bytes = syn::literal::Bytes<'cx, Self>;
    type HexBytes = syn::literal::HexBytes<'cx, Self>;
    type DateTime = syn::literal::DateTime<'cx, Self>;
    type Numeric = syn::literal::Numeric<'cx, Self>;
    type Apply = syn::Apply<'cx, Self>;
    type Unary = syn::Unary<'cx, Self>;
    type Binary = syn::Binary<'cx, Self>;
    type Compare = syn::Compare<'cx, Self>;
    type Set = syn::Set<'cx, Self>;
    type InfixImport = syn::InfixImport<'cx, Self>;
    type If = syn::If<'cx, Self>;
    type FunctionDef = syn::FunctionDef<'cx, Self>;
    type ConstantDef = syn::ConstantDef<'cx, Self>;
};

/// Generate type definitions for use within the opslang-ast crate itself.
pub fn v1_default_type_subst_internal(input: TypeSubstitution) -> Result<TokenStream, syn::Error> {
    generate_type_subst(input, "crate")
}

/// Generate type definitions for use in external crates.
pub fn v1_default_type_subst(input: TypeSubstitution) -> Result<TokenStream, syn::Error> {
    generate_type_subst(input, "::opslang_ast")
}

fn generate_type_subst(
    substitution: TypeSubstitution,
    crate_prefix: &str,
) -> Result<TokenStream, syn::Error> {
    let mut type_definitions = Vec::new();

    for (name, default_type_path) in DEFAULT_TYPES {
        let name_ident = syn::parse_str::<Ident>(name)?;

        // Check if this type is overridden
        if let Some(override_type) = substitution.overrides.iter().find(|o| o.name == name_ident) {
            let name = &override_type.name;
            let ty = &override_type.ty;
            type_definitions.push(quote! {
                type #name = #ty;
            });
        } else {
            // Replace relative paths with absolute paths
            let full_type_path =
                default_type_path.replace("syn::", &format!("{crate_prefix}::syntax::v1::"));
            let default_type_tokens: TokenStream =
                syn::parse_str(&full_type_path).map_err(|e| {
                    syn::Error::new_spanned(
                        &name_ident,
                        format!("Failed to parse default type `{full_type_path}`: {e}"),
                    )
                })?;
            type_definitions.push(quote! {
                type #name_ident = #default_type_tokens;
            });
        }
    }

    Ok(quote! {
        #(#type_definitions)*
    })
}
