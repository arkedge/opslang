//! Compile-time consistency check for IR types registry.
//!
//! This module performs a compile-time verification that all IR types defined in the
//! centralized registry (`opslang_ir_macro::ir_types`) actually exist and are accessible
//! from this v1 child module context using appropriate paths.
//!
//! # Purpose
//!
//! The IR types registry in `opslang-ir-macro/src/visitor_type_registry.rs` maintains a comprehensive
//! list of all IR-related node types from multiple crates (opslang-ast, opslang-ir, opslang-ty)
//! for use by procedural macros. However, this registry is independent of the actual type
//! definitions across these crates, creating a potential source of inconsistency.
//!
//! This design weakness cannot be avoided due to architectural constraints, but this
//! compile-time check helps detect inconsistencies early through compilation errors
//! and comments that guide developers when types are added, removed, or renamed.
//!
//! # Implementation
//!
//! The macro call below generates compile-time type checks in the form:
//! ```ignore
//! const _: () = {
//!     fn check<'cx>() {
//!         let _: super::Program<'cx, super::IrTypeFamily>;
//!         let _: super::Expr<'cx>;
//!         let _: super::ResolvedPath<'cx>;
//!         let _: ::opslang_ast::syntax::v1::Comment<'cx>;
//!         let _: ::opslang_ty::version::v1::Ty<'cx>;
//!         // ... for each type in the registry
//!     }
//! };
//! ```
//!
//! # Guarantees
//!
//! Successful compilation of this module ensures:
//! 1. **Cross-crate Type Existence**: All types in the IR registry exist across referenced crates
//! 2. **Path Accessibility**: All types are accessible from v1 child module context using appropriate paths
//! 3. **Type Family Compatibility**: All types accept the expected lifetime and type family parameters
//! 4. **Registry Consistency**: The centralized type registry matches actual type definitions across multiple crates
//!
//! # Maintenance
//!
//! When adding, removing, or renaming IR-related types across any crate:
//! 1. Update the actual type definitions in the respective crates (opslang-ast, opslang-ir, opslang-ty)
//! 2. Update the registry in `opslang-ir-macro/src/visitor_type_registry.rs`
//! 3. Ensure this compilation check continues to pass
//!
//! If this check fails, it indicates a mismatch between the registry and actual definitions
//! that must be resolved to maintain consistency across the entire IR ecosystem.

// Note: This macro call is skipped during test compilation because doctests
// change the module context, causing `super` paths in the generated code
// to fail resolution. The consistency check still runs during normal builds.
#[cfg(not(test))]
opslang_ir_macro::ir_consistency_check!();
