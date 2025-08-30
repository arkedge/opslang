use crate::ir_types::IrType;
use opslang_visitor_macro_helper::{
    MethodKind, VisitorMode,
    shared_visitor_trait::{TraitDeclaration, VisitableType, declare_visitor_trait},
};

impl VisitableType for IrType {
    fn generate_visit_method_name(&self, kind: MethodKind, mode: VisitorMode) -> String {
        self.generate_visit_method_name(kind, mode)
    }

    fn full_type_path(&self) -> proc_macro2::TokenStream {
        let type_path = self.inside_of_v1_child_mod().super_path();
        quote::quote! { #type_path }
    }
}

/// Generates a comprehensive immutable visitor trait for all V1 IR types.
pub fn declare_ir_visitor_trait(
    trait_decl: TraitDeclaration,
) -> syn::Result<proc_macro2::TokenStream> {
    let all_ir_types = IrType::get_v1_ir_types();
    declare_visitor_trait(trait_decl, all_ir_types, VisitorMode::Visit)
}

/// Generates a comprehensive mutable visitor trait for all V1 IR types.
pub fn declare_ir_visitor_mut_trait(
    trait_decl: TraitDeclaration,
) -> syn::Result<proc_macro2::TokenStream> {
    let all_ir_types = IrType::get_v1_ir_types();
    declare_visitor_trait(trait_decl, all_ir_types, VisitorMode::VisitMut)
}
