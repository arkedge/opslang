//! Visitor implementation consistency tests for IR types.
//!
//! This test file validates that the IR visitor implementation macros can generate
//! valid code for empty visitor definitions. This serves as a structural consistency
//! check for the IR type registry and visitor implementation logic across multiple
//! crates (opslang-ast, opslang-ir, opslang-ty).
//!
//! # Purpose
//!
//! Empty visitor definitions are particularly valuable for testing because they
//! exercise the complete visitor generation machinery without any custom logic:
//!
//! 1. **Cross-crate Type Registry Validation**: All types in `ir_types.rs` from
//!    multiple crates must exist and be accessible for the macro to generate
//!    visitor methods.
//!
//! 2. **Method Generation Logic**: The visitor implementation macro must correctly
//!    generate method signatures and default implementations for all IR types,
//!    including proper type family substitutions.
//!
//! 3. **Path Resolution**: All type paths used in generated code must resolve
//!    correctly from the visitor implementation context, including external
//!    crate references.
//!
//! # Failure Scenarios
//!
//! If any test in this file fails to compile, it indicates one of these issues:
//!
//! - **Registry Inconsistency**: A type listed in `ir_types.rs` doesn't exist
//!   or has been renamed/moved without updating the registry across any of the
//!   referenced crates (opslang-ast, opslang-ir, opslang-ty).
//!
//! - **Visitor Implementation Bug**: The `visitor_impl.rs` macro has a bug in
//!   method signature generation, path construction, or code generation logic
//!   for cross-crate type references.
//!
//! - **Type Path Issues**: Generated visitor methods reference types using
//!   incorrect paths that don't resolve in the current module context, especially
//!   for types from external crates.
//!
//! - **Type Family Issues**: IR type family substitutions are incorrect, causing
//!   type parameter mismatches in generated visitor methods.
//!
//! These tests complement the compile-time consistency checks by validating
//! the end-to-end visitor generation pipeline rather than just type existence.

/// Empty IR visitor to test basic immutable visitor generation without custom implementations.
///
/// This visitor defines no custom visit methods, relying entirely on the macro
/// to generate all required visitor trait implementations. Successful compilation
/// indicates that:
///
/// 1. All IR types in the registry are accessible and properly defined across crates
/// 2. The visitor macro can generate syntactically correct method signatures
/// 3. Default traversal implementations compile without errors
/// 4. Type paths resolve correctly in the generated code for cross-crate references
/// 5. Type family substitutions work correctly for AST types used in IR context
struct EmptyIrVisitor;

opslang_ir_macro::v1_ir_visitor_impl!(for EmptyIrVisitor {
    // Intentionally empty - this tests that the macro can generate
    // complete visitor implementations with no custom methods
});

/// Empty mutable IR visitor to test basic mutable visitor generation.
///
/// This visitor tests the mutable visitor generation pipeline, ensuring that
/// mutable references and traversal work correctly across all IR types.
struct EmptyIrMutVisitor;

opslang_ir_macro::v1_ir_visitor_impl!(for EmptyIrMutVisitor {
    // Intentionally empty - this tests that the macro can generate
    // complete mutable visitor implementations with no custom methods
});

/// Test that empty IR visitors implement the expected immutable visitor trait.
///
/// This test validates that the generated visitor properly implements
/// the IrVisitor trait, ensuring the macro generates complete and correct
/// implementations for immutable traversal.
#[test]
fn empty_ir_visitor_trait_implementation() {
    use opslang_ir::version::v1::visit::IrVisitor;

    let visitor = EmptyIrVisitor;

    // Test that the visitor implements IrVisitor trait
    // We can't actually call visit methods without constructing IR nodes,
    // but we can verify the trait is implemented
    fn _assert_implements_ir_visitor<'cx, T: IrVisitor<'cx>>(_: T) {}
    _assert_implements_ir_visitor(visitor);
}

/// Test that empty IR visitors implement the expected mutable visitor trait.
///
/// This test validates that the generated visitor properly implements
/// the IrVisitorMut trait, ensuring the macro generates complete and correct
/// implementations for mutable traversal.
#[test]
fn empty_ir_mut_visitor_trait_implementation() {
    use opslang_ir::version::v1::visit::IrMutVisitor;

    let visitor = EmptyIrMutVisitor;

    // Test that the visitor implements IrMutVisitor trait
    // We can't actually call visit methods without constructing IR nodes,
    // but we can verify the trait is implemented
    fn _assert_implements_ir_mut_visitor<'cx, T: IrMutVisitor<'cx>>(_: T) {}
    _assert_implements_ir_mut_visitor(visitor);
}