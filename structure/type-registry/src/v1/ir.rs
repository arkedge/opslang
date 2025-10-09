//! IR Visitor Type Registry - Multi-crate type registry and interfaces for IR visitor generation.
//!
//! # Purpose
//!
//! This module serves as the **comprehensive hub** for IR visitor type management across
//! multiple crates. It handles the complex orchestration of types from:
//!
//! - **AST types** (`opslang-ast`): With type family substitution (default or IR)
//! - **IR types** (`opslang-ir`): Native IR representations  
//! - **Ty types** (`opslang-ty`): Type system representations
//!
//! # Registry Constants (Multi-Crate Registry)
//!
//! - [`visitor_type_registry::NODE_TYPES`]: Complete registry spanning all three crates
//! - [`visitor_type_registry::INTER_TYPES`]: Intermediate types with wrapper and external type support
//!
//! These const-defined registries manage the **complex multi-crate type ecosystem** for IR.
//!
//! # Core Type Interfaces
//!
//! - [`types::IrNodeTy`]: Multi-crate type descriptor with type family substitution
//! - [`types::IrInterTy`]: Advanced intermediate type with wrapper support
//! - [`types::InstanceKind`]: Crate origin and type family specification
//! - [`types::TypeSubstitution`]: Type family substitution control
//!
//! # Design Principle
//!
//! **When structures change in `opslang-ast`, `opslang-ir`, or `opslang-ty`, you will almost
//! certainly need to update these registries.**
//!
//! This visitor system requires **complete type coverage** across all three crates: every type
//! appearing in IR-context definitions must be handled by one of three mechanisms:
//! 1. **Node types** (defined in this registry with proper crate mapping)
//! 2. **Intermediate types** (defined in this registry with wrapper support)
//! 3. **Generic types** (handled by `opslang-visitor-macro-helper`)
//!
//! The multi-crate nature adds complexity through type family substitutions and crate classifications,
//! but this macro-driven approach still provides **significantly easier maintenance** than manual
//! visitor implementation updates.
//!
//! # Consistency Verification
//!
//! **Incomplete coverage is automatically detected**: If any type is missing from these registries,
//! compilation will fail in `opslang-ir/tests/visitor_consistency.rs`, providing clear error
//! messages about which types need to be added to the registry.
//!
//! # Module Structure
//!
//! - [`types`]: Multi-crate wrapper types and path generators
//! - [`const_compatible`]: Const-time construction for complex type hierarchies

use proc_macro2::Span;
use std::{borrow::Cow, fmt::Debug};
use syn::{
    AngleBracketedGenericArguments, GenericArgument, Ident, Path, Token, parse_quote,
    punctuated::Punctuated,
};

pub mod const_compatible;
pub mod types;
pub mod visitor_type_registry;

mod macro_def;
