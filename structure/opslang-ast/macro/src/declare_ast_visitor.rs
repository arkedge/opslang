use opslang_visitor_macro_helper::{
    VisitorMode,
    shared_visitor_trait::{TraitDeclaration, declare_visitor_trait},
};

use opslang_type_registry::v1::ast::types::AstNodeTy;

/// Generates a comprehensive visitor trait for all V1 AST types.
pub fn declare_ast_visitor_trait(
    trait_decl: TraitDeclaration,
) -> syn::Result<proc_macro2::TokenStream> {
    let all_ast_types = AstNodeTy::get_v1_ast_node_types();
    declare_visitor_trait::<AstNodeTy>(trait_decl, all_ast_types, VisitorMode::Visit)
}
