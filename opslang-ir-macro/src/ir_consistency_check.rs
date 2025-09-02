use proc_macro2::TokenStream;
use quote::quote;

/// Generates a compile-time consistency check that verifies all IR types defined in `visitor_type_registry.rs`
/// actually exist and are accessible from a v1 child module.
///
/// This macro creates a compile-time check that ensures type consistency between the centralized
/// type registry in `visitor_type_registry.rs` and the actual type definitions across multiple crates
/// (opslang-ast, opslang-ir, opslang-ty).
///
/// The generated code uses `const _: () = { let _: T; }` pattern to verify that each type
/// can be referenced and is valid at compile time. If any type in the registry doesn't exist
/// or isn't accessible, compilation will fail with a clear error message.
///
/// See the call site for more information.
pub fn ir_consistency_check() -> TokenStream {
    let ir_types = crate::visitor_type_registry::IrNodeTy::get_v1_ir_node_types();

    let type_checks = ir_types.map(|ir_type| {
        let type_path = ir_type.inside_of_v1_child_mod().super_path();
        quote! {
            let _: #type_path;
        }
    });

    quote! {
        const _: () = {
            fn check<'cx>() {
                #(#type_checks)*
            }
        };
    }
}
