//! AST Visitor Type Registry - Core type registry and interfaces for visitor generation.
//!
//! # Purpose
//!
//! This module serves as the **central hub** for AST visitor type management, containing:
//!
//! 1. **Type Registry**: Complete const-defined registries of all visitable AST types
//! 2. **Core Interfaces**: Primary type descriptors used by macro generation
//! 3. **Utility Modules**: Supporting infrastructure for type management
//!
//! # Registry Constants (The Heart of the System)
//!
//! - [`visitor_type_registry::NODE_TYPES`]: Complete registry of main AST node types
//! - [`visitor_type_registry::INTER_TYPES`]: Registry of intermediate/utility types
//!
//! These const-defined registries are the **single source of truth** for visitor generation.
//!
//! # Core Type Interfaces  
//!
//! - [`types::AstNodeTy`]: Individual AST type descriptor (macro generation interface)
//! - [`types::AstInterTy`]: Intermediate type descriptor
//!
//! # Design Principle & Maintenance
//!
//! **When AST structures change in `opslang-ast`, you will almost certainly need to update these registries.**
//!
//! This visitor system requires **complete type coverage**: every type appearing in AST definitions
//! must be handled by one of three mechanisms:
//! 1. **Node types** (defined in this registry)
//! 2. **Intermediate types** (defined in this registry)
//! 3. **Generic types** (handled by `opslang-visitor-macro-helper`)
//!
//! Types marked with `skip_visit` attributes are automatically excluded from visitor generation.
//!
//! This macro-driven design makes maintenance **significantly easier** than manually editing
//! visitor implementations for every structural change.
//!
//! # Consistency Verification
//!
//! **Incomplete coverage is automatically detected**: If any type is missing from these registries,
//! compilation will fail in `opslang-ast/tests/visitor_consistency.rs`, providing clear error
//! messages about which types need to be added to the registry.
//!
//! # Module Structure
//!
//! - [`types`]: Multi-crate wrapper types and path generators
//! - [`const_compatible`]: Const-time construction for complex type hierarchies

use syn::{Ident, Path, parse_quote};

pub mod const_compatible;
pub mod types;
pub mod visitor_type_registry;

mod macro_def;
