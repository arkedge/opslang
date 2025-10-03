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
//! - [`V1_IR_NODE_TYPES`]: Complete registry spanning all three crates
//! - [`V1_IR_INTER_TYPES`]: Intermediate types with wrapper and external type support
//!
//! These const-defined registries manage the **complex multi-crate type ecosystem** for IR.
//!
//! # Core Type Interfaces
//!
//! - [`IrNodeTy`]: Multi-crate type descriptor with type family substitution
//! - [`IrInterTy`]: Advanced intermediate type with wrapper support
//! - [`InstanceKind`]: Crate origin and type family specification
//! - [`TypeSubstitution`]: Type family substitution control
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
//! - [`type_descriptors`]: Multi-crate wrapper types and path generators
//! - [`const_compatible`]: Const-time construction for complex type hierarchies

use std::{borrow::Cow, fmt::Debug};

use proc_macro2::Span;
use syn::{
    AngleBracketedGenericArguments, GenericArgument, Ident, Path, Token, parse_quote,
    punctuated::Punctuated,
};

pub mod const_compatible;
mod type_descriptors;
use const_compatible::*;

use type_descriptors::{
    InsideV1ChildModTy, InstanceKind, IrInterTyInstance, IrNodeTyInstance, OutsideIrCrateInterTy,
    OutsideIrCrateTy, TypeSubstitution,
};

/// Represents a type in the IR that can be instantiated with a lifetime.
///
/// # Invariants
///
/// This struct represents types from both `opslang-ast` and `opslang-ir` crates
/// that can be instantiated with a lifetime parameter, conventionally `'cx`.
///
/// The primary invariants are:
/// 1. The combination of `name`, `module_path`, and `crate_name` must form a valid path
///    to a type definition (e.g., `::opslang_ast::syntax::v1::Stmt` or `::opslang_ir::version::v1::Comment`).
/// 2. The referenced type must be a generic type that accepts exactly one lifetime parameter.
/// 3. The type must be instantiable when provided with the `'cx` lifetime.
///
/// This ensures that the procedural macros using this struct can correctly generate
/// code that references these lifetime-annotated IR types.
/// Represents an IR node type from AST, IR, or Ty crates that can be instantiated with a lifetime.
///
/// Can represent types from `opslang-ast`, `opslang-ir`, or `opslang-ty` crates with proper
/// type family substitution and path qualification.
#[derive(Debug)]
pub struct IrNodeTy<P: ExecPhase = Runtime> {
    name: Mapped<Ident, P>,
    child: Option<Mapped<Ident, P>>,
    ty: Mapped<type_descriptors::IrNodeTyInstance, P>,
}

impl IrNodeTy {
    /// Convert to a super-qualified type for relative references.
    ///
    /// This creates a type that generates paths like `super::Type<'cx>` or `super::module::Type<'cx>`
    /// for use in contexts where relative references within the same crate are appropriate.
    pub const fn inside_of_v1_child_mod(&self) -> InsideV1ChildModTy<'_> {
        InsideV1ChildModTy { inner: self }
    }

    /// Convert to a crate-qualified type for external references.
    ///
    /// This creates a type that generates paths like `::opslang_ir::version::v1::Type<'cx>`
    /// for use in contexts where the full crate path is needed.
    pub const fn outside_of_ir_crate(&self) -> OutsideIrCrateTy<'_> {
        OutsideIrCrateTy { inner: self }
    }

    /// Returns all IR node types for v1 syntax including both AST and IR specific types.
    pub fn get_v1_ir_node_types() -> impl Iterator<Item = Self> {
        V1_IR_NODE_TYPES.iter().map(|ty| ty.parse())
    }
}

/// Represents intermediate IR types that may be instance-based or external paths.
#[derive(Debug)]
pub enum IrInterTy<P: ExecPhase = Runtime> {
    /// IR type instance with configurable generics.
    Instance(IrInterTyInstance<P>),
    /// External type path (like std types).
    External(Mapped<Path, P>),
}

impl IrInterTy {
    /// Convert to a crate-qualified type for external references.
    pub const fn outside_of_ir_crate(&self) -> OutsideIrCrateInterTy<'_> {
        OutsideIrCrateInterTy { inner: self }
    }

    /// Returns all intermediate IR types for v1 syntax.
    pub fn get_v1_ir_inter_types() -> impl Iterator<Item = Self> {
        V1_IR_INTER_TYPES.iter().map(|ty| ty.parse())
    }
}

/// Macro for defining IR node types from multiple crates with type family support.
///
/// Supports AST types with both default and IR type families, plus native IR and Ty types.
macro_rules! define_ir_node_types {
    (
        crate ast<'cx, ir> {
            $(type $ast_name:ident;)*
            $(mod $ast_module:ident {
                $(type $ast_mod_name:ident;)*
            })*
        }
        crate ast<'cx, default> {
            $(type $ast_default_name:ident;)*
        }
        crate ir<'cx> {
            $(type $ir_name:ident;)*
        }
        crate ty<'cx> {
            $(type $ty_name:ident;)*
        }
        crate ty {
            $(type $ty_no_cx_name:ident;)*
        }
    ) => {
        {
            let default = IrNodeTy {
                name: "!",
                child: None,
                ty: None,
            };
            &[
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::ast_ir(),
                            has_lifetime: true,
                        }),
                        name: stringify!($ast_name),
                        ..default
                    },
                )*
                $(
                    $(
                        IrNodeTy {
                            ty: Some(IrNodeTyInstance {
                                ty: InstanceKind::ast_ir(),
                                has_lifetime: true,
                            }),
                            child: Some(stringify!($ast_module)),
                            name: stringify!($ast_mod_name),
                        },
                    )*
                )*
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::ast_default(),
                            has_lifetime: true,
                        }),
                        name: stringify!($ast_default_name),
                        ..default
                    },
                )*
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::ir(),
                            has_lifetime: true,
                        }),
                        name: stringify!($ir_name),
                        ..default
                    },
                )*
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::ty(),
                            has_lifetime: true,
                        }),
                        name: stringify!($ty_name),
                        ..default
                    },
                )*
                $(
                    IrNodeTy {
                        ty: Some(IrNodeTyInstance {
                            ty: InstanceKind::ty(),
                            has_lifetime: false,
                        }),
                        name: stringify!($ty_no_cx_name),
                        ..default
                    },
                )*
            ]
        }
    };
}

/// Macro for defining intermediate IR types with wrapper and external type support.
macro_rules! define_ir_inter_types {
    (
        crate ast {
            $(type $ast_name:ident $(<$ast_cx:lifetime, $ast_subst:ident>)? $(: $ast_wrapper:path)?;)*
        }
        crate ir {
            $(type $ir_name:ident $(<$ir_cx:lifetime>)? $(: $ir_wrapper:path)?;)*
        }
        crate ty {
            $(type $ty_name:ident $(<$ty_cx:lifetime>)? $(: $ty_wrapper:path)?;)*
        }
        extern {
            $(type $ext_name:path;)*
        }
    ) => {
        {
            let default = IrInterTyInstance {
                name: "!",
                child: None,
                ty: None,
                has_lifetime: false,
                wrapper: None,
            };
            &[
                $(
                    IrInterTy::Instance(IrInterTyInstance {
                        ty: Some(InstanceKind::Ast({
                            #[allow(unused_mut, unused_assignments)]
                            let mut kind = TypeSubstitution::Default;
                            $(
                                kind = TypeSubstitution::$ast_subst();
                            )?
                            kind
                        })),
                        name: stringify!($ast_name),
                        $(
                            has_lifetime: {stringify!($ast_cx); true},
                        )?
                        $(
                            wrapper: Some(stringify!($ast_wrapper)),
                        )?
                        ..default
                    }),
                )*
                $(
                    IrInterTy::Instance(IrInterTyInstance {
                        ty: Some(InstanceKind::ir()),
                        name: stringify!($ir_name),
                        $(
                            has_lifetime: {stringify!($ir_cx); true},
                        )?
                        $(
                            wrapper: Some(stringify!($ir_wrapper)),
                        )?
                        ..default
                    }),
                )*
                $(
                    IrInterTy::Instance(IrInterTyInstance {
                        ty: Some(InstanceKind::ty()),
                        name: stringify!($ty_name),
                        $(
                            has_lifetime: {stringify!($ty_cx); true},
                        )?
                        $(
                            wrapper: Some(stringify!($ty_wrapper)),
                        )?
                        ..default
                    }),
                )*
                $(
                    IrInterTy::External(stringify!($ext_name)),
                )*
            ]
        }
    };
}

/// Complete registry of all IR node types for v1 syntax from multiple crates.
///
/// # Maintenance Guide
///
/// **CRITICAL**: This registry spans THREE crates. When you modify types:
///
/// ## AST Types (`opslang-ast`)
/// - **With IR type family**: Add to `crate ast<'cx, ir>` section
/// - **With default type family**: Add to `crate ast<'cx, default>` section
/// - Choose based on whether the type should use `IrTypeFamily` or `DefaultTypeFamily`
///
/// ## IR Types (`opslang-ir`)
/// - Add to `crate ir<'cx>` section
/// - These are native IR representations
///
/// ## Ty Types (`opslang-ty`)
/// - Add to `crate ty<'cx>` section  
/// - These are type system representations
///
/// **Incomplete coverage**: Will cause compilation failures in `opslang-ir/tests/visitor_consistency.rs`
const V1_IR_NODE_TYPES: &[IrNodeTy<Const>] = define_ir_node_types! {
    // types that are defined in ast crate, substituted with 'cx and ir type family
    crate ast<'cx, ir> {
        // actual type is `Program<'cx, IrTypeFamily>`
        type Program;
        type ToplevelItem;
        type DefinitionKind;
        type FunctionDef;
        type Parameter;
        type ConstantDef;
        type ScopeItem;
        type Row;
        type Block;
        type Statement;
        type Let;
        type ExprStatement;
        type ReturnStmt;
        type Qualif;
        type Modifier;
        type ModifierParam;
        type DefaultModifier;
        type PreQualified;
        type Unary;
        type UnOp;
        type CompareOp;
        type NotEqualToken;
        type Binary;
        type BinOp;
        type Set;
        type Cast;
        type InfixImport;
        type If;
        type IfElse;
        type Call;
        type Wait;
        type Select;

        // Literal types
        type Literal;
        type Array;

        // Token types
        mod token {
            // actual type is `Semi<'cx, IrTypeFamily>`
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
            type Call;
            type Wait;
            type Select;
            type Prc;
            type Const;
        }
    }
    // types that are defined in ast crate, substituted with 'cx and default type family
    crate ast<'cx, default> {
        // actual type is `Comment<'cx>`
        type Comment;
        type Path;
        type NumericSuffix;
        type Ident;

        type String;
        type Bytes;
        type HexBytes;
        type DateTime;
        type Numeric;
    }
    // types that are defined in ir crate, substituted with 'cx
    crate ir<'cx> {
        // actual type is `Comment<'cx>`
        type Comment;
        type Definition;
        type ResolvedPath;
        type ResolvedItem;
        type Expr;
        type Scope;
        type String;
        type Bytes;
        type HexBytes;
        type DateTime;
        type Numeric;
        type Apply;
        type Compare;
    }
    // types that are defined in ty crate, substituted with 'cx
    crate ty<'cx> {
        type Ty; // actual type is `Ty<'cx>`, and so on
        type ModuleItem;
    }
    // types that are defined in ty crate, without 'cx
    crate ty {
        type InferTy; // actual type is `InferTy`, and so on
        type TyVid;
        type IntVid;
        type FloatVid;
    }
};

/// Registry of intermediate IR types with multi-crate and wrapper support.
///
/// # Maintenance Guide
///
/// **Advanced registry**: Supports wrapper types (like `Vec<T>`) and external types.
///
/// ## Adding Types
/// - **AST intermediate**: Add to `crate ast` with optional lifetime/substitution
/// - **IR intermediate**: Add to `crate ir` with optional lifetime  
/// - **Ty intermediate**: Add to `crate ty` with optional lifetime
/// - **External types**: Add to `extern` (like `std::convert::Infallible`)
/// - **Wrapped types**: Use `: WrapperType` syntax (like `: ::std::vec::Vec`)
///
/// **When structures change**: You will almost certainly need to update this registry.
/// **Incomplete coverage**: Will cause compilation failures in `opslang-ir/tests/visitor_consistency.rs`
const V1_IR_INTER_TYPES: &[IrInterTy<Const>] = define_ir_inter_types! {
    crate ast {
        type DefaultTypeFamily;
        type Span;
        type BytePos;
        type NumericKind;
        type ExprKind<'cx, ir>;
        type ExprMut<'cx, ir>;
        type ScopeItem<'cx, ir>: ::std::vec::Vec; // actual type is Vec<ScopeItem<'cx, IrTypeFamily>>
        type Qualif<'cx, ir>: ::std::vec::Vec;
        type SelectItem<'cx, ir>;
        type SelectItem<'cx, ir>: ::std::vec::Vec;
    }
    crate ir {
        type IrTypeFamily;
        type BinOp<'cx>;
        type NumericKind<'cx>;
        type Expr<'cx>: ::std::vec::Vec;
        type CompareOpExpr<'cx>;
        type CompareOpExpr<'cx>: ::std::vec::Vec; // actual type is Vec<CompareOpExpr<'cx, IrTypeFamily>
    }
    crate ty {
        type Ident<'cx>;
        type TyKind<'cx>;
        type Procedure<'cx>;
        type Identifier<'cx>;
        type Ty<'cx>: ::std::vec::Vec; // actual type is Vec<Ty<'cx>>
        type IntTy;
        type UintTy;
        type FloatTy;
        type ModuleItemDef<'cx>;
    }
    extern {
        type ::std::convert::Infallible;
        type ::chrono::DateTime<::chrono::Utc>;
    }
};

/// Functions for macro.
impl type_descriptors::InstanceKind {
    /// Create AST instance with IR type family substitution.
    const fn ast_ir() -> Self {
        Self::Ast(type_descriptors::TypeSubstitution::Ir)
    }
    /// Create AST instance with default type family.
    const fn ast_default() -> Self {
        Self::Ast(type_descriptors::TypeSubstitution::Default)
    }
    /// Create IR crate instance.
    const fn ir() -> Self {
        Self::Ir
    }
    /// Create Ty crate instance.
    const fn ty() -> Self {
        Self::Ty
    }
}

/// Functions for macro.
impl type_descriptors::TypeSubstitution {
    /// Create default type substitution.
    #[allow(dead_code)]
    const fn default() -> Self {
        Self::Default
    }
    /// Create IR type substitution.
    const fn ir() -> Self {
        Self::Ir
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ident_safe() {
        let _ir_node_types = IrNodeTy::get_v1_ir_node_types();
        let _ir_inter_types = IrInterTy::get_v1_ir_inter_types();
    }
}
