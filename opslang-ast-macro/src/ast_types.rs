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
#[derive(Clone, Debug)]
pub struct AstType {
    name: &'static str,
    module_path: Option<&'static str>,
}

impl AstType {
    pub const fn new(name: &'static str) -> Self {
        Self {
            name,
            module_path: None,
        }
    }

    pub const fn with_module(name: &'static str, module_path: &'static str) -> Self {
        Self {
            name,
            module_path: Some(module_path),
        }
    }

    /// Generate full path for this type with proper module qualification.
    pub fn full_path(&self) -> String {
        match self.module_path {
            Some(module) => format!("syntax::v1::{module}::{}<'cx>", self.name),
            None => format!("syntax::v1::{}<'cx>", self.name),
        }
    }

    /// Generate identifier-safe name for method generation, avoiding conflicts.
    pub fn ident_safe_name(&self) -> String {
        match self.module_path {
            Some(module) => format!("{module}_{}", self.name),
            None => self.name.to_string(),
        }
    }

    /// Generate syn path for this type with proper module qualification.
    pub fn type_path(&self) -> proc_macro2::TokenStream {
        use quote::quote;
        let type_ident = syn::Ident::new(self.name, proc_macro2::Span::call_site());
        match self.module_path {
            Some(module) => {
                let module_ident = syn::Ident::new(module, proc_macro2::Span::call_site());
                quote! { super::#module_ident::#type_ident<'cx> }
            }
            None => quote! { super::#type_ident<'cx> },
        }
    }

    /// Generate appropriate visit method name based on the type.
    /// If module_path exists, generates `visit_{module}_{name}`, otherwise `visit_{name}`.
    pub fn generate_visit_method_name(&self) -> String {
        use convert_case::{Case, Casing};

        let snake_name = self.name.to_case(Case::Snake);

        if let Some(module) = self.module_path {
            let snake_module = module.to_case(Case::Snake);
            format!("visit_{snake_module}_{snake_name}")
        } else {
            format!("visit_{snake_name}")
        }
    }

    /// Returns all AST types for v1 syntax including token types.
    pub const fn get_v1_ast_types() -> &'static [AstType] {
        V1_AST_TYPES
    }
}

/// A macro to define AST types in a more Rust-like syntax with module grouping.
macro_rules! define_ast_types {
    (
        $(type $name:ident;)*
        $(mod $module:ident {
            $(type $mod_name:ident;)*
        })*
    ) => {
        &[
            $(AstType::new(stringify!($name)),)*
            $($(AstType::with_module(stringify!($mod_name), stringify!($module))),*)*
        ]
    };
}

/// All AST types for v1 syntax including token types.
const V1_AST_TYPES: &[AstType] = define_ast_types! {
    // Main AST types
    type Program;
    type Definition;
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
    type Literal;
    type Array;
    type String;
    type Bytes;
    type HexBytes;
    type Numeric;
    type NumericSuffix;
    type DateTime;
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
    type InfixImport;
    type If;
    type IfElse;

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
};
