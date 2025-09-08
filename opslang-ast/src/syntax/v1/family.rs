use std::fmt::Debug;

/// Shorthand for repeating trait constraints with optional documentation.
macro_rules! declare_family {
    ($($(#[$attr:meta])* type $ident:ident $(: $tr:ident)?;)*) => {
        $(
            $(#[$attr])*
            type $ident: std::fmt::Debug + PartialEq $(+ $tr)*;
        )*
    };
}

/// Type family that occurs in almost every AST types to allow global type substitution.
/// This pattern is known as "trees that grow" and allows for flexible AST transformations.
///
/// # Design Overview
///
/// This trait enables the same AST structure to be used with different concrete types
/// across different phases of compilation (parsing, type checking, IR generation, etc.).
/// Each phase can provide its own TypeFamily implementation with appropriate concrete types.
///
/// This trait takes a lifetime parameter `'cx` so that all associated types can refer to it
/// uniformly, enabling arena-based memory allocation patterns.
///
/// # Adding New Types
///
/// When adding a new associated type to this trait, you **MUST** update all of the following:
///
/// 1. **Core implementation in `opslang-ast`**:
///    - Add the type to this trait definition
///    - Add corresponding parameter to [`opslang_ast_macro::v1_default_type_subst!`] macro
///    - Add implementation in the macro body to map to concrete type
///    - Update any enums/structs to use `F::YourType` instead of `YourType<'cx, F>`
///
/// 2. **Printer support in `opslang-printer`**:
///    - Add the type to `PrintableFamily` trait definition
///    - Add the type to `PrintableFamily` implementation (the impl block before the trait)
///
/// 3. **IR support in `opslang-ir`**:
///    - Add the type to `TypeFamily` implementation (maps to IR version of the type)
///
/// 4. **Migration support in `opslang-migration`** (if applicable):
///    - Update conversion code if the type is used in v0 to v1 migration
///
/// # Type Categories
///
/// The associated types are organized into logical groups:
/// - **Foundational**: `Span`, `Position` - source location information
/// - **Structural**: `Comment`, `Row`, `Block`, etc. - document structure
/// - **Names**: `Ident`, `Path` - identifier and path resolution
/// - **Expressions**: `Expr` and all expression-related types
/// - **Definitions**: Top-level constructs like function and constant definitions
///
/// [`v1_default_type_subst!`]: opslang_ast_macro::v1_default_type_subst
pub trait TypeFamily<'cx>: Debug + PartialEq + Clone + Copy + Default + 'static {
    declare_family! {
        // === Foundational Types ===
        /// Source location span representing a range in the source code (start/end positions).
        ///
        /// It is useful to implement [`super::token::IntoSpan`] for conversion into this type.
        type Span: Copy;

        /// Single source position (line, column) in the source code.
        ///
        /// It is useful to implement [`super::token::IntoPosition`] for conversion into this type.
        type Position: Copy;

        // === Structural Types ===
        /// Comments attached to AST nodes, preserving documentation and annotations.
        type Comment;

        type ToplevelItem;

        type Scope;

        /// A single row/line in the source code.
        type Row;

        /// Content of a row, typically containing a statement or expression.
        type Statement;

        /// Block of code containing a sequence of scope items (statements, nested blocks).
        type Block;

        /// Return statement that yields a value from a function or block.
        type ReturnStmt;

        // === Names and Identifiers ===
        /// Simple identifier used for variable names, function names, etc.
        type Ident: Copy;

        /// Path to an identifier, which may include module qualification or scope resolution.
        type Path: Copy;

        type Ty;

        type FnReturnTy;

        // === Expression Types ===
        /// Base expression type representing the main expression enum that contains all expression variants.
        type Expr;

        type Exprs;

        /// Literal values including numbers, strings, arrays, and other constant data.
        type Literal;

        /// Array literal containing a sequence of expressions.
        type Array;

        /// String literal value.
        type String;

        /// Byte sequence literal value.
        type Bytes;

        /// Hexadecimal byte sequence literal value.
        type HexBytes;

        /// Date-time literal value.
        type DateTime;

        /// Parenthesized expressions that group sub-expressions and control precedence.
        type Parened;

        /// Qualification expressions such as attributes, decorators, and metadata annotations.
        type Qualif;

        /// Pre-qualified expressions that have qualifications applied before the main expression.
        type PreQualified;

        /// Numeric literal values (integers, floating-point numbers).
        type Numeric;

        /// Function application/call expressions that invoke functions with arguments.
        type Apply;

        /// Unary operations including negation (-), reference (&), dereference ($), etc.
        type Unary;

        /// Binary operations including arithmetic (+, -, *, /, %), logical (&&, ||), etc.
        type Binary;

        /// Comparison operations including equality (==, !=) and relational (<, >, <=, >=).
        type Compare;

        /// Assignment/set operations using the `:=` operator.
        type Set;

        /// Infix import operations that import from a file using the `file?path` syntax.
        type InfixImport;

        /// Conditional expressions with if/then/else branching logic.
        type If;

        /// Async select expression from multiple awaitable expressions.
        type Select;

        type SelectItems;

        // === Definition Types ===
        /// Function definition declared with `prc` keyword, including parameters and body.
        type FunctionDef;

        /// Constant definition declared with `const` keyword, binding a name to a value.
        type ConstantDef;
    }
}
