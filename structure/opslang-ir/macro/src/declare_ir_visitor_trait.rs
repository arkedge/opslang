use opslang_type_registry::v1::ir::types::IrNodeTy;
use opslang_visitor_macro_helper::{
    VisitorMode,
    shared_visitor_trait::{TraitDeclaration, declare_visitor_trait},
};

/// Generates a comprehensive immutable visitor trait for all V1 IR types.
pub fn declare_ir_visitor_trait(
    trait_decl: TraitDeclaration,
) -> syn::Result<proc_macro2::TokenStream> {
    let all_ir_types = IrNodeTy::get_v1_ir_node_types();
    declare_visitor_trait(trait_decl, all_ir_types, VisitorMode::Visit)
}

/// Generates a comprehensive mutable visitor trait for all V1 IR types.
pub fn declare_ir_visitor_mut_trait(
    trait_decl: TraitDeclaration,
) -> syn::Result<proc_macro2::TokenStream> {
    let all_ir_types = IrNodeTy::get_v1_ir_node_types();
    declare_visitor_trait(trait_decl, all_ir_types, VisitorMode::VisitMut)
}
