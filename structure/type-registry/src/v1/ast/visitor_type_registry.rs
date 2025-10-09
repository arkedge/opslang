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
pub const NODE_TYPES: &[AstNodeTy<Const>] = crate::define_v1_ast_node_types! {
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
        type CompareOpExpr;
        type NotEqualToken;
        type Binary;
        type BinOp;
        type Apply;
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
            type Call;
            type Wait;
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
pub const INTER_TYPES: &[AstInterTy<Const>] = crate::define_v1_ast_inter_types! {
    crate ast {
        type DefaultTypeFamily;
        type Span;
        type BytePos;
        type NumericKind;
        type ExprKind<'cx>;
        type SelectItem<'cx>;
    }
};

use super::*;
use const_compatible::*;
use types::*;
