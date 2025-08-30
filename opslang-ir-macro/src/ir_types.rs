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

    /// AST type with module and default TypeFamily (no type substitution)
    pub const fn with_ast_module_default(name: &'static str, module_path: &'static str) -> Self {
        Self {
            name,
            module_path: Some(module_path),
            instance: Instance::Ast(TypeSubstitution::Default), // Use default TypeFamily
        }
    }

    /// Generate full path for this type with proper crate and module qualification.
    fn full_path(&self) -> String {
        let (crate_prefix, type_suffix) = match self.instance {
            Instance::Ir => ("::opslang_ir::version::v1", "<'cx>"),
            Instance::Ty => ("::opslang_ty::version::v1", "<'cx>"),
            Instance::Ast(TypeSubstitution::Ir) => (
                "::opslang_ast::syntax::v1",
                "<'cx, ::opslang_ir::version::v1::IrTypeFamily>",
            ),
            Instance::Ast(TypeSubstitution::Default) => ("::opslang_ast::syntax::v1", "<'cx>"),
        };

        match self.module_path {
            Some(module) => format!("{crate_prefix}::{module}::{}{type_suffix}", self.name),
            None => format!("{crate_prefix}::{}{type_suffix}", self.name),
        }
    }

    /// Generate appropriate visit method name based on the type and visitor mode.
    /// If module_path exists, generates `visit_{crate}_{module}_{name}` or `visit_{crate}_{module}_{name}_mut`,
    /// otherwise `visit_{crate}_{name}` or `visit_{crate}_{name}_mut`.
    pub fn generate_visit_method_name(
        &self,
        mode: opslang_visitor_macro_helper::VisitorMode,
    ) -> String {
        use convert_case::{Case, Casing};

        let snake_name = self.name.to_case(Case::Snake);
        // let crate_prefix = match self.instance {
        //     Instance::Ast(_) => "ast",
        //     Instance::Ir => "ir",
        //     Instance::Ty => "ty",
        // };
        let method_suffix = match mode {
            opslang_visitor_macro_helper::VisitorMode::Visit => "",
            opslang_visitor_macro_helper::VisitorMode::VisitMut => "_mut",
        };

        if let Some(module) = self.module_path {
            let snake_module = module.to_case(Case::Snake);
            format!("visit_{snake_module}_{snake_name}{method_suffix}")
        } else {
            format!("visit_{snake_name}{method_suffix}")
        }
    }

    /// Returns all IR types for v1 syntax including both AST and IR specific types.
    pub const fn get_v1_ir_types() -> &'static [IrType] {
        V1_IR_NODE_TYPES
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
        self.inner.full_path()
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
        type Expr;
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
        mod literal {
            type String;
            type Bytes;
            type HexBytes;
            type DateTime;
            type Numeric;
        }
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
