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
#[derive(Clone, Debug)]
pub struct IrType {
    name: &'static str,
    module_path: Option<&'static str>,
    instance: Instance,
}

#[derive(Clone, Copy, Debug)]
enum Instance {
    Ast(TypeSubstitution),
    Ir,
    Ty,
}

#[derive(Clone, Copy, Debug)]
enum TypeSubstitution {
    Default, // Use default TypeFamily (no substitution)
    Ir,      // Use IrTypeFamily
}

impl IrType {
    pub const fn new_ast(name: &'static str) -> Self {
        Self {
            name,
            module_path: None,
            instance: Instance::Ast(TypeSubstitution::Ir), // AST types default to IrTypeFamily in IR context
        }
    }

    pub const fn new_ir(name: &'static str) -> Self {
        Self {
            name,
            module_path: None,
            instance: Instance::Ir,
        }
    }

    pub const fn new_ty(name: &'static str) -> Self {
        Self {
            name,
            module_path: None,
            instance: Instance::Ty,
        }
    }

    pub const fn with_ast_module(name: &'static str, module_path: &'static str) -> Self {
        Self {
            name,
            module_path: Some(module_path),
            instance: Instance::Ast(TypeSubstitution::Ir), // AST types default to IrTypeFamily in IR context
        }
    }

    /// AST type with default TypeFamily (no type substitution)
    pub const fn new_ast_default(name: &'static str) -> Self {
        Self {
            name,
            module_path: None,
            instance: Instance::Ast(TypeSubstitution::Default), // Use default TypeFamily
        }
    }

    /// Generate appropriate visit method name based on the type, method kind, and visitor mode.
    /// If module_path exists, generates `{prefix}_{module}_{name}[_mut]`, otherwise `{prefix}_{name}[_mut]`.
    pub fn generate_visit_method_name(
        &self,
        kind: opslang_visitor_macro_helper::MethodKind,
        mode: opslang_visitor_macro_helper::VisitorMode,
    ) -> String {
        use convert_case::{Case, Casing};

        let mut string;
        let prefix = match kind {
            opslang_visitor_macro_helper::MethodKind::Visit => "visit_",
            opslang_visitor_macro_helper::MethodKind::Super => "super_",
        };
        string = prefix.to_string();

        if let Instance::Ast(TypeSubstitution::Default) = self.instance {
            string.push_str("ast_");
        }

        if let Some(module) = self.module_path {
            string.push_str(&module.to_case(Case::Snake));
            string.push('_');
        };

        string.push_str(&self.name.to_case(Case::Snake));

        if let opslang_visitor_macro_helper::VisitorMode::VisitMut = mode {
            string.push_str("_mut");
        }

        string
    }

    /// Returns all IR types for v1 syntax including both AST and IR specific types.
    pub const fn get_v1_ir_types() -> &'static [IrType] {
        V1_IR_NODE_TYPES
    }

    /// Generate full path for this type with proper crate and module qualification.
    ///
    /// When `use_super_for_ir` is true, IR crate types use "super" instead of absolute paths.
    fn full_path(&self, use_super_for_ir: bool) -> String {
        let crate_prefix = match self.instance {
            Instance::Ir => {
                if use_super_for_ir {
                    "super"
                } else {
                    "::opslang_ir::version::v1"
                }
            }
            Instance::Ty => "::opslang_ty::version::v1",
            Instance::Ast(_) => "::opslang_ast::syntax::v1",
        };
        let type_suffix = match self.instance {
            Instance::Ir => "<'cx>",
            Instance::Ty => "<'cx>",
            Instance::Ast(TypeSubstitution::Default) => "<'cx>",
            Instance::Ast(TypeSubstitution::Ir) => {
                if use_super_for_ir {
                    "<'cx, super::IrTypeFamily>"
                } else {
                    "<'cx, ::opslang_ir::version::v1::IrTypeFamily>"
                }
            }
        };

        let name = self.name;
        match self.module_path {
            Some(module) => format!("{crate_prefix}::{module}::{name}{type_suffix}"),
            None => format!("{crate_prefix}::{name}{type_suffix}"),
        }
    }

    /// Convert to a super-qualified type for relative references.
    ///
    /// This creates a type that generates paths like `super::Type<'cx>` or `super::module::Type<'cx>`
    /// for use in contexts where relative references within the same crate are appropriate.
    pub const fn inside_of_v1_child_mod(&self) -> InsideV1ChildModType<'_> {
        InsideV1ChildModType { inner: self }
    }

    /// Convert to a crate-qualified type for external references.
    ///
    /// This creates a type that generates paths like `::opslang_ir::version::v1::Type<'cx>`
    /// for use in contexts where the full crate path is needed.
    pub const fn outside_of_ir_crate(&self) -> OutsideIrCrateType<'_> {
        OutsideIrCrateType { inner: self }
    }
}

/// A macro to define IR types in a more Rust-like syntax with module grouping and crate separation.
macro_rules! define_ir_node_types {
    (
        crate ast<ir> {
            $(type $ast_name:ident;)*
            $(mod $ast_module:ident {
                $(type $ast_mod_name:ident;)*
            })*
        }
        crate ast<default> {
            $(type $ast_default_name:ident;)*
            $(mod $ast_default_module:ident {
                $(type $ast_default_mod_name:ident;)*
            })*
        }
        crate ir {
            $(type $ir_name:ident;)*
        }
        crate ty {
            $(type $ty_name:ident;)*
        }
    ) => {
        &[
            $(IrType::new_ast(stringify!($ast_name)),)*
            $($(IrType::with_ast_module(stringify!($ast_mod_name), stringify!($ast_module)),)*)*

            $(IrType::new_ast_default(stringify!($ast_default_name)),)*
            $($(IrType::with_ast_module_default(stringify!($ast_default_mod_name), stringify!($ast_default_module)),)*)*

            $(IrType::new_ir(stringify!($ir_name)),)*

            $(IrType::new_ty(stringify!($ty_name)),)*
        ]
    };
}

/// A newtyped wrapper for [`IrType`] that ensures super-qualified paths.
///
/// This type generates paths like `super::Type<'cx>` or `super::module::Type<'cx>` and is
/// intended for contexts where relative references within the same crate are appropriate,
/// such as in trait declarations.
#[derive(Clone, Debug)]
pub struct InsideV1ChildModType<'a> {
    inner: &'a IrType,
}

impl InsideV1ChildModType<'_> {
    /// Generate super-qualified token stream path.
    ///
    /// Returns a [`proc_macro2::TokenStream`] representing paths like `super::Type<'cx>`
    /// suitable for relative references within the same crate.
    pub fn super_path(&self) -> proc_macro2::TokenStream {
        let path_string = self.inner.full_path(true);
        path_string
            .parse()
            .expect("Generated path should be valid Rust syntax")
    }
}

/// A newtyped wrapper for [`IrType`] that ensures crate-qualified paths.
///
/// This type generates paths like `::opslang_ir::version::v1::Type<'cx>` and is intended
/// for contexts where external crate references are needed, such as in visitor implementations
/// that reference types from outside the current crate.
#[derive(Clone, Debug)]
pub struct OutsideIrCrateType<'a> {
    inner: &'a IrType,
}

impl OutsideIrCrateType<'_> {
    /// Generate fully qualified path with crate prefix.
    ///
    /// Returns a string like `::opslang_ir::version::v1::Type<'cx>` suitable for
    /// external references to IR types.
    pub fn full_crate_path(&self) -> String {
        self.inner.full_path(false)
    }
}

/// All IR types for v1 syntax including both AST and IR specific types.
const V1_IR_NODE_TYPES: &[IrType] = define_ir_node_types! {
    // types that are defined in ast crate and substituted with ir type family
    crate ast<ir> {
        type Program;
        type Definition;
        type DefinitionKind;
        type FunctionDef;
        type Parameter;
        type ConstantDef;
        type Scope;
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
        type Compare;
        type CompareOp;
        type NotEqualToken;
        type Binary;
        type BinOp;
        type Set;
        type InfixImport;
        type If;
        type IfElse;

        // Literal types
        type Literal;
        type Array;

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
            type Return;
            type Let;
            type If;
            type Else;
            type Prc;
            type Const;
            type In;
        }
    }
    // types that are defined in ast crate and substituted with default type family
    crate ast<default> {
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
    // types that are defined in ir crate
    crate ir {
        type Comment;
        type ResolvedPath;
        type Expr;
        type String;
        type Bytes;
        type HexBytes;
        type DateTime;
        type Numeric;
        type Apply;
    }
    // types that are defined in ty crate
    crate ty {
        type Ty;
        type ModuleItem;
    }
};
