//! Visitor implementation consistency tests.
//!
//! This test file validates that the visitor implementation macros can generate
//! valid code for empty visitor definitions. This serves as a structural consistency
//! check for the AST type registry and visitor implementation logic.
//!
//! # Purpose
//!
//! Empty visitor definitions are particularly valuable for testing because they
//! exercise the complete visitor generation machinery without any custom logic:
//!
//! 1. **Type Registry Validation**: All types in `ast_types.rs` must exist and be
//!    accessible for the macro to generate visitor methods.
//!
//! 2. **Method Generation Logic**: The visitor implementation macro must correctly
//!    generate method signatures and default implementations for all AST types.
//!
//! 3. **Path Resolution**: All type paths used in generated code must resolve
//!    correctly from the visitor implementation context.
//!
//! # Failure Scenarios
//!
//! If any test in this file fails to compile, it indicates one of these issues:
//!
//! - **Registry Inconsistency**: A type listed in `ast_types.rs` doesn't exist
//!   or has been renamed/moved without updating the registry.
//!
//! - **Visitor Implementation Bug**: The `visitor_impl.rs` macro has a bug in
//!   method signature generation, path construction, or code generation logic.
//!
//! - **Type Path Issues**: Generated visitor methods reference types using
//!   incorrect paths that don't resolve in the current module context.
//!
//! These tests complement the compile-time consistency checks by validating
//! the end-to-end visitor generation pipeline rather than just type existence.

/// Empty AST visitor to test basic visitor generation without custom implementations.
///
/// This visitor defines no custom visit methods, relying entirely on the macro
/// to generate all required visitor trait implementations. Successful compilation
/// indicates that:
///
/// 1. All AST types in the registry are accessible and properly defined
/// 2. The visitor macro can generate syntactically correct method signatures
/// 3. Default traversal implementations compile without errors
/// 4. Type paths resolve correctly in the generated code
struct EmptyAstVisitor;

opslang_ast_macro::v1_ast_visitor_impl!(for EmptyAstVisitor {
    // Intentionally empty - this tests that the macro can generate
    // complete visitor implementations with no custom methods
});

/// Test that an empty AST visitor implements the expected traits.
///
/// This test validates that the generated visitor properly implements
/// all necessary visitor traits, ensuring the macro generates complete
/// and correct implementations.
#[test]
fn empty_ast_visitor_trait_implementation() {
    use opslang_ast::v1::visit::AstVisitor;

    let visitor = EmptyAstVisitor;

    // Test that the visitor implements AstVisitor trait
    // We can't actually call visit methods without constructing AST nodes,
    // but we can verify the trait is implemented
    fn _assert_implements_ast_visitor<'cx, T: AstVisitor<'cx>>(_: T) {}
    _assert_implements_ast_visitor(visitor);
}
