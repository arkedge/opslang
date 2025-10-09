use opslang_visitor_macro_helper::{
    MethodKind, VisitorMode,
    shared_visitor_trait::{TraitDeclaration, VisitableType, declare_visitor_trait},
};
use quote::quote;

use crate::visitor_type_registry::AstNodeTy;

impl VisitableType for AstNodeTy {
    fn generate_visit_method_name(&self, kind: MethodKind, _mode: VisitorMode) -> String {
        self.generate_visit_method_name(kind)
    }

    fn full_type_path(&self) -> proc_macro2::TokenStream {
        let super_qualified = self.inside_of_v1_child_mod();
        let type_path = super_qualified.super_path();
        quote! { #type_path }
    }
}

/// Generates a comprehensive visitor trait for all V1 AST types.
pub fn declare_ast_visitor_trait(
    trait_decl: TraitDeclaration,
) -> syn::Result<proc_macro2::TokenStream> {
    let all_ast_types = crate::visitor_type_registry::AstNodeTy::get_v1_ast_node_types();
    declare_visitor_trait::<AstNodeTy>(trait_decl, all_ast_types, VisitorMode::Visit)
}
