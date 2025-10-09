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
pub const NODE_TYPES: &[IrNodeTy<Const>] = crate::define_ir_node_types! {
    // types that are defined in ast crate, substituted with 'cx and ir type family
    crate ast<'cx, ir> {
        type Program; // actual type is `Program<'cx, IrTypeFamily>`
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
            type Semi; // actual type is `Semi<'cx, IrTypeFamily>`
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
        type Comment; // actual type is `Comment<'cx>`
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
        type Comment; // actual type is `Comment<'cx>`
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
        type Ty; // actual type is `Ty<'cx>`
    }
    // types that are defined in ty crate, without 'cx
    crate ty {
        type InferTy; // actual type is `InferTy`, and so on
        type TyVid;
        type IntVid;
        type FloatVid;
    }
    // types that are defined in module crate, substituted with 'cx
    crate module<'cx> {
        type ModuleItem; // actual type is `ModuleItem<'cx>`
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
pub const INTER_TYPES: &[IrInterTy<Const>] = crate::define_ir_inter_types! {
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
    }
    crate module {
        type ModuleItemDef<'cx>;
    }
    extern {
        type ::std::convert::Infallible;
        type ::chrono::DateTime<::chrono::Utc>;
    }
};

use super::*;
use const_compatible::*;
use types::*;
