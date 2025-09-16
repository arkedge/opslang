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
//! - [`V1_AST_NODE_TYPES`]: Complete registry of main AST node types
//! - [`V1_AST_INTER_TYPES`]: Registry of intermediate/utility types
//!
//! These const-defined registries are the **single source of truth** for visitor generation.
//!
//! # Core Type Interfaces  
//!
//! - [`AstNodeTy`]: Individual AST type descriptor (macro generation interface)
//! - [`AstInterTy`]: Intermediate type descriptor
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
//! - [`type_descriptors`]: Wrapper types and utility methods
//! - [`const_compatible`]: Const-time type construction support

pub mod const_compatible;
mod type_descriptors;

use const_compatible::*;
use syn::{Ident, Path, parse_quote};

use crate::visitor_type_registry::type_descriptors::{
    InsideV1ChildModTy, OutsideAstCrateInterTy, OutsideAstCrateTy,
};

/// Represents a generic type in the AST that can be instantiated with a lifetime.
///
/// # Invariants
///
/// This struct represents a generic type from the `opslang-ast` crate that can be
/// instantiated with a lifetime parameter, conventionally `'cx`.
///
/// The primary invariants are:
/// 1. The combination of `name` and `module_path` must form a valid path to a type
///    definition within the `opslang-ast` crate (e.g., `crate::syntax::v1::Stmt`).
/// 2. The referenced type must be a generic type that accepts exactly one lifetime
///    parameter.
/// 3. The type must be instantiable when provided with the `'cx` lifetime. For instance,
///    if the type is `path::to::TypeName`, then `path::to::TypeName<'cx>` must be a
///    valid, constructible type.
///
/// This ensures that the procedural macros using this struct can correctly generate
/// code that references these lifetime-annotated AST types.
///
/// # Context-specific types
///
/// Different usage contexts have different requirements for path generation.
/// Use the context-specific newtypes instead of accessing `full_path()` or `type_path()` directly:
/// - [`OutsideAstCrateTy`] for external crate references
/// - [`InsideV1ChildModTy`] for relative references within the same crate
#[derive(Debug)]
pub struct AstNodeTy<P: ExecPhase = Runtime> {
    name: Mapped<Ident, P>,
    child: Option<Mapped<Ident, P>>,
}

impl AstNodeTy {
    /// Convert to a crate-qualified type for external references.
    ///
    /// This creates a type that generates paths like `::opslang_ast::syntax::v1::Type<'cx>`
    /// for use in contexts where the full crate path is needed.
    pub const fn outside_of_ast_crate(&self) -> OutsideAstCrateTy<'_> {
        OutsideAstCrateTy { inner: self }
    }

    /// Convert to a super-qualified type for relative references.
    ///
    /// This creates a type that generates paths like `super::Type<'cx>` or `super::module::Type<'cx>`
    /// for use in trait declarations within the same crate.
    pub const fn inside_of_v1_child_mod(&self) -> InsideV1ChildModTy<'_> {
        InsideV1ChildModTy { inner: self }
    }

    /// Returns all AST types for v1 syntax including token types.
    pub fn get_v1_ast_node_types() -> impl Iterator<Item = Self> {
        V1_AST_NODE_TYPES.iter().map(|ty| ty.parse())
    }
}

/// Represents an intermediate AST type that may or may not have a lifetime parameter.
///
/// These are typically utility types like `Span`, `BytePos`, or `NumericKind` used
/// throughout the AST but not part of the main visitor pattern.
#[derive(Debug)]
pub struct AstInterTy<P: ExecPhase = Runtime> {
    name: Mapped<Ident, P>,
    has_lifetime: bool,
}

impl AstInterTy {
    /// Convert to a crate-qualified type for external references.
    pub const fn outside_of_ast_crate(&self) -> OutsideAstCrateInterTy<'_> {
        OutsideAstCrateInterTy { inner: self }
    }

    /// Returns all intermediate AST types for v1 syntax.
    pub fn get_v1_ast_node_types() -> impl Iterator<Item = Self> {
        V1_AST_INTER_TYPES.iter().map(|ty| ty.parse())
    }
}

/// Macro for defining AST node types using Rust-like syntax.
///
/// Supports both top-level types and module-grouped types.
macro_rules! define_ast_node_types {
    (
        crate ast<'cx> {
            $(type $name:ident;)*
            $(mod $module:ident {
                $(type $mod_name:ident;)*
            })*
        }
    ) => {
        &[
            $(
                AstNodeTy {
                    name: stringify!($name),
                    child: None,
                },
            )*
            $(
                $(
                    AstNodeTy {
                        name: stringify!($mod_name),
                        child: Some(stringify!($module))
                    },
                )*
            )*
        ]
    };
}

/// Macro for defining intermediate AST types with optional lifetime parameters.
macro_rules! define_ast_inter_types {
    (
        crate ast {
            $(type $name:ident $(<$cx:lifetime>)?;)*
        }
    ) => {
        &[
            $({
                #[allow(unused_mut)]
                let mut t = AstInterTy {
                    name: stringify!($name),
                    has_lifetime: false,
                };
                $(
                    stringify!($cx);
                    t.has_lifetime = true;
                )?
                t
            },)*
        ]
    };
}

/// Complete registry of all AST node types for v1 syntax.
///
/// # Maintenance Guide
///
/// **IMPORTANT**: When you add, remove, or rename types in `opslang-ast/src/syntax/v1/`:
///
/// 1. **Adding a type**: Add it to the appropriate section below (main types or token module)
/// 2. **Removing a type**: Remove it from this list
/// 3. **Renaming a type**: Update the name here to match
/// 4. **Moving to/from a module**: Update the module structure accordingly
///
/// This registry drives visitor method generation - missing types won't have visitor methods,
/// and stale entries will cause compilation errors.
const V1_AST_NODE_TYPES: &[AstNodeTy<Const>] = define_ast_node_types! {
    crate ast<'cx> {
        // Main types
        type Program;
        type ToplevelItem;
        type DefinitionKind;
        type FunctionDef;
        type Parameter;
        type FnReturnTy;
        type ConstantDef;
        type Scope;
        type ScopeItem;
        type Row;
        type Comment;
        type Block;
        type Statement;
        type Let;
        type ExprStatement;
        type ReturnStmt;
        type Expr;
        type Path;
        type Ident;
        type Qualif;
        type Modifier;
        type ModifierParam;
        type DefaultModifier;
        type Parened;
        type PreQualified;
        type Unary;
        type UnOp;
        type Compare;
        type CompareOp;
        type NotEqualToken;
        type Binary;
        type BinOp;
        type Apply;
        type Set;
        type Cast;
        type InfixImport;
        type If;
        type IfElse;
        type Select;

        // Literal types
        type Literal;
        type Array;
        type String;
        type Bytes;
        type HexBytes;
        type Numeric;
        type NumericSuffix;
        type DateTime;

        // Token types
        mod token {
            type Semi;
            type Break;
            type Atmark;
            type Tilde;
            type Colon;
            type Eq;
            type OpenBrace;
            type CloseBrace;
            type OpenParen;
            type CloseParen;
            type OpenSquare;
            type CloseSquare;
            type Hyphen;
            type Ampersand;
            type Dollar;
            type Question;
            type RightAngle;
            type Angle;
            type Star;
            type Slash;
            type Percent;
            type Plus;
            type BangEqual;
            type SlashEqual;
            type EqualEqual;
            type RightAngleEq;
            type AngleEq;
            type ColonEq;
            type AndAnd;
            type OrOr;
            type Arrow;
            type DoubleArrow;
            type Return;
            type Let;
            type As;
            type If;
            type Else;
            type Select;
            type Prc;
            type Const;
        }
    }
};

/// Registry of intermediate AST types for v1 syntax.
///
/// # Maintenance Guide
///
/// These are utility types like `Span`, `BytePos`, `NumericKind` that need visitor
/// implementations but are not primary AST nodes.
///
/// **When structures change**: You will almost certainly need to update this registry.
/// **Incomplete coverage**: Will cause compilation failures in `opslang-ast/tests/visitor_consistency.rs`
const V1_AST_INTER_TYPES: &[AstInterTy<Const>] = define_ast_inter_types! {
    crate ast {
        type DefaultTypeFamily;
        type Span;
        type BytePos;
        type NumericKind;
        type ExprKind<'cx>;
        type SelectItem<'cx>;
    }
};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ident_safe() {
        let _ast_node_types = AstNodeTy::get_v1_ast_node_types();
    }
}
