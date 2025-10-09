use proc_macro2::TokenStream;
use quote::quote;

/// Generates a compile-time consistency check that verifies all AST types defined in `ast_types.rs`
/// actually exist and are accessible from a v1 child module.
///
/// This macro creates a compile-time check that ensures type consistency between the centralized
/// type registry in `ast_types.rs` and the actual type definitions in `opslang-ast/src/syntax/v1.rs`.
///
/// The generated code uses `const _: () = { let _: T; }` pattern to verify that each type
/// can be referenced and is valid at compile time. If any type in the registry doesn't exist
/// or isn't accessible, compilation will fail with a clear error message.
///
/// See the call site for more information.
pub fn ast_consistency_check() -> TokenStream {
    let ast_types = opslang_type_registry::v1::ast::types::AstNodeTy::get_v1_ast_node_types();

    let type_checks = ast_types.map(|ast_type| {
        let type_path = ast_type.inside_of_v1_child_mod().super_path();
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
