//! Compile-time consistency check for AST types registry.
//!
//! This module performs a compile-time verification that all AST types defined in the
//! centralized registry (`opslang_ast_macro::visitor_type_registry`) actually exist and are accessible
//! from this v1 child module context using `super::` paths.
//!
//! # Purpose
//!
//! The AST types registry in `opslang-ast-macro/src/visitor_type_registry.rs` maintains a comprehensive
//! list of all AST node types for use by procedural macros. However, this registry is
//! independent of the actual type definitions in `opslang-ast/src/syntax/v1.rs`, creating
//! a potential source of inconsistency.
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
//!     let _: super::Program<'cx>;
//!     let _: super::ToplevelItem<'cx>;
//!     let _: super::token::Semi<'cx>;
//!     // ... for each type in the registry
//! };
//! ```
//!
//! # Guarantees
//!
//! Successful compilation of this module ensures:
//! 1. **Type Existence**: All types in the AST registry actually exist in the codebase
//! 2. **Path Accessibility**: All types are accessible from v1 child module context using `super::` paths
//! 3. **Lifetime Compatibility**: All types accept the expected lifetime parameter `'cx`
//! 4. **Registry Consistency**: The centralized type registry matches the actual type definitions
//!
//! # Maintenance
//!
//! When adding, removing, or renaming AST types:
//! 1. Update the actual type definitions in `v1.rs` and related files
//! 2. Update the registry in `opslang-ast-macro/src/visitor_type_registry.rs`
//! 3. Ensure this compilation check continues to pass
//!
//! If this check fails, it indicates a mismatch between the registry and actual definitions
//! that must be resolved to maintain consistency across the codebase.

opslang_ast_macro::ast_consistency_check!();
