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
/// - [`CrateQualifiedType`] for external crate references
/// - [`SuperQualifiedType`] for relative references within the same crate
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
    ///
    /// # Private
    ///
    /// This method is private to prevent direct usage. Use [`AstType::as_crate_qualified`]
    /// or [`AstType::as_super_qualified`] to get context-appropriate types.
    fn full_path(&self) -> syn::Path {
        let str = match self.module_path {
            Some(module) => format!("::opslang_ast::syntax::v1::{module}::{}<'cx>", self.name),
            None => format!("::opslang_ast::syntax::v1::{}<'cx>", self.name),
        };
        syn::parse_str(&str).unwrap()
    }

    /// Generate syn path for this type with proper module qualification.
    ///
    /// # Private
    ///
    /// This method is private to prevent direct usage. Use [`AstType::as_crate_qualified`]
    /// or [`AstType::as_super_qualified`] to get context-appropriate types.
    fn type_path(&self) -> proc_macro2::TokenStream {
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

    /// Generate appropriate method name based on the type and method kind.
    /// If module_path exists, generates `{prefix}_{module}_{name}`, otherwise `{prefix}_{name}`.
    pub fn generate_visit_method_name(
        &self,
        kind: opslang_visitor_macro_helper::MethodKind,
    ) -> String {
        use convert_case::{Case, Casing};

        let snake_name = self.name.to_case(Case::Snake);
        let prefix = match kind {
            opslang_visitor_macro_helper::MethodKind::Visit => "visit",
            opslang_visitor_macro_helper::MethodKind::Super => "super",
        };

        if let Some(module) = self.module_path {
            let snake_module = module.to_case(Case::Snake);
            format!("{prefix}_{snake_module}_{snake_name}")
        } else {
            format!("{prefix}_{snake_name}")
        }
    }

    /// Returns all AST types for v1 syntax including token types.
    pub const fn get_v1_ast_node_types() -> &'static [AstType] {
        V1_AST_NODE_TYPES
    }

    /// Convert to a crate-qualified type for external references.
    ///
    /// This creates a type that generates paths like `::opslang_ast::syntax::v1::Type<'cx>`
    /// for use in contexts where the full crate path is needed.
    pub const fn outside_of_ast_crate(&self) -> OutsideAstCrateType<'_> {
        OutsideAstCrateType { inner: self }
    }

    /// Convert to a super-qualified type for relative references.
    ///
    /// This creates a type that generates paths like `super::Type<'cx>` or `super::module::Type<'cx>`
    /// for use in trait declarations within the same crate.
    pub const fn inside_of_v1_child_mod(&self) -> InsideV1ChildModType<'_> {
        InsideV1ChildModType { inner: self }
    }
}

/// A macro to define AST types in a more Rust-like syntax with module grouping.
macro_rules! define_ast_node_types {
    (
        $(type $name:ident;)*
        $(mod $module:ident {
            $(type $mod_name:ident;)*
        })*
    ) => {
        &[
            $(AstType::new(stringify!($name)),)*
            $($(AstType::with_module(stringify!($mod_name), stringify!($module)),)*)*
        ]
    };
}

/// A newtyped wrapper for [`AstType`] that ensures crate-qualified paths.
///
/// This type generates paths like `::opslang_ast::syntax::v1::Type<'cx>` and is intended
/// for contexts where external crate references are needed, such as in visitor implementations
/// that reference types from outside the current crate.
#[derive(Clone, Debug)]
pub struct OutsideAstCrateType<'a> {
    inner: &'a AstType,
}

impl OutsideAstCrateType<'_> {
    /// Generate fully qualified path with crate prefix.
    ///
    /// Returns a string like `::opslang_ast::syntax::v1::Type<'cx>` suitable for
    /// external references to AST types.
    pub fn full_crate_path(&self) -> syn::Path {
        self.inner.full_path()
    }
}

/// A newtyped wrapper for [`AstType`] that ensures super-qualified paths.
///
/// This type generates paths like `super::Type<'cx>` or `super::module::Type<'cx>` and is
/// intended for contexts where relative references within the same crate are appropriate,
/// such as in trait declarations.
#[derive(Clone, Debug)]
pub struct InsideV1ChildModType<'a> {
    inner: &'a AstType,
}

impl InsideV1ChildModType<'_> {
    /// Generate super-qualified token stream path.
    ///
    /// Returns a [`proc_macro2::TokenStream`] representing paths like `super::Type<'cx>`
    /// suitable for relative references within the same crate.
    pub fn super_path(&self) -> proc_macro2::TokenStream {
        self.inner.type_path()
    }
}

/// All AST types for v1 syntax including token types.
const V1_AST_NODE_TYPES: &[AstType] = define_ast_node_types! {
    // Main AST types
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
};
