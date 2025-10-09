/*!
Type system for opslang v1.

This module defines the type system structures including type kinds, identifiers, and
module management for the opslang v1 implementation.

**IMPORTANT**: When modifying type structures in this module, update the visitor type
registry in `opslang-ir-macro/src/visitor_type_registry.rs` to ensure proper visitor
macro generation. Choose whether to make types hookable (add to node types) or not
hookable (add to inter types).
*/

use opslang_visitor_macro::Visit;
use std::fmt::Display;
use std::hash::Hash;
use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};
use typed_arena::Arena;

use opslang_ast::syntax::v1 as ast;

pub mod hm;
pub use hm::Substitution;

mod infer;
pub use infer::{FloatVid, InferTy, IntVid, TyVid};

pub mod constructor;

pub mod context;
pub use context::TypingContext;

mod poly;
pub use poly::PolyTy;

/// Represents the different kinds of types in the type system.
///
/// This enum defines all possible type variants that can exist in the language,
/// including primitive types, compound types, and type variables for inference.
#[derive(Debug, Clone, PartialEq, Visit)]
pub enum TyKind<'cx> {
    /// Signed integer types.
    Int(IntTy),

    /// Unsigned integer types.
    Uint(UintTy),

    /// Floating point types.
    Float(FloatTy),

    /// String type for text data.
    String,

    /// Byte array type for binary data.
    Bytes,

    /// Boolean type for true/false values.
    Bool,

    /// Duration type for time intervals.
    Duration,

    /// Time type for specific points in time.
    Time,

    /// Array type containing elements of a specific inner type.
    Array { inner: Ty<'cx> },

    /// Function type with argument types and return type.
    Function {
        arg: Vec<Ty<'cx>>,
        ret: Ty<'cx>,

        /// Whether this is a procedure.
        is_procedure: Option<Procedure<'cx>>,
    },

    /// Inference variable used during type inference.
    Infer(InferTy),

    /// Unit type representing no meaningful value.
    Unit,

    /// External type identified by a string name.
    External { path: ast::Path<'cx>, ty: Ty<'cx> },
}

/// A type reference that points to a type kind.
///
/// This is a lightweight wrapper around a reference to TyKind that allows
/// for efficient sharing of type information across the type checker.
#[derive(Debug, Clone, Copy, PartialEq, Visit)]
pub struct Ty<'cx>(pub &'cx TyKind<'cx>);

impl<'cx> Ty<'cx> {
    /// Returns a reference to the underlying type kind.
    ///
    /// This provides access to the actual type information stored within the type reference.
    pub fn kind(&self) -> &'cx TyKind<'cx> {
        self.0
    }
}

impl<'cx> std::ops::Deref for Ty<'cx> {
    type Target = &'cx TyKind<'cx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for Ty<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

/// Signed integer types, following Rust's naming convention.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
}

/// Unsigned integer types, following Rust's naming convention.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
}

/// Floating point types, following Rust's naming convention.
#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(Debug, Clone, Copy, PartialEq, Visit)]
pub enum Procedure<'cx> {
    SameModule { name: Ident<'cx> },
    // FIXME: Add cross-module procedures
    External,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Integer {
    Int(IntTy),
    Uint(UintTy),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Numeric {
    Int(IntTy),
    Uint(UintTy),
    Float(FloatTy),
}

/// Represents an identifier with scope information.
///
/// Identifiers are used to distinguish variables and functions across different scopes,
/// allowing proper name resolution in nested contexts.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub struct Identifier<'cx> {
    /// The string name of the identifier
    pub name: &'cx str,
    /// The unique definition ID for this identifier
    pub definition_id: usize,
}

/// A reference to an identifier.
///
/// This is a lightweight wrapper around an Identifier reference that enables
/// efficient passing and comparison of identifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
pub struct Ident<'cx>(pub &'cx Identifier<'cx>);

impl<'cx> std::ops::Deref for Ident<'cx> {
    type Target = &'cx Identifier<'cx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cx> TyKind<'cx> {
    /// Performs the occurs check for the given type variable.
    ///
    /// This check prevents infinite types by ensuring a type variable doesn't
    /// occur within its own definition during unification.
    pub fn occurs(&self, var: TyVid) -> bool {
        match self {
            // Direct variable occurrence
            TyKind::Infer(InferTy::TyVar(v)) => *v == var,
            // IntVar and FloatVar cannot occur in general type variables
            TyKind::Infer(InferTy::IntVar(_) | InferTy::FloatVar(_)) => false,
            // Recursively check array element type
            TyKind::Array { inner } => inner.occurs(var),
            // Check all argument types and return type
            TyKind::Function {
                arg,
                ret,
                is_procedure: _,
            } => arg.iter().any(|arg| arg.occurs(var)) || ret.occurs(var),
            // Primitive types cannot contain variables
            _ => false,
        }
    }

    /// Returns `true` if the ty kind is [`Bool`].
    ///
    /// [`Bool`]: TyKind::Bool
    #[must_use]
    pub fn is_bool(&self) -> bool {
        matches!(self, Self::Bool)
    }

    /// Returns `true` if the ty kind is [`Duration`].
    ///
    /// [`Duration`]: TyKind::Duration
    #[must_use]
    pub fn is_duration(&self) -> bool {
        matches!(self, Self::Duration)
    }

    /// Returns `true` if the ty kind is [`Time`].
    ///
    /// [`Time`]: TyKind::Time
    #[must_use]
    pub fn is_time(&self) -> bool {
        matches!(self, Self::Time)
    }

    /// Returns `true` if the ty kind is [`Unit`].
    ///
    /// [`Unit`]: TyKind::Unit
    #[must_use]
    pub fn is_unit(&self) -> bool {
        matches!(self, Self::Unit)
    }

    #[must_use]
    pub fn as_numeric(&self) -> Option<Numeric> {
        match self {
            TyKind::Int(int_ty) => Some(Numeric::Int(*int_ty)),
            TyKind::Uint(uint_ty) => Some(Numeric::Uint(*uint_ty)),
            TyKind::Float(float_ty) => Some(Numeric::Float(*float_ty)),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_numeric(&self) -> bool {
        matches!(self, TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_))
    }

    #[must_use]
    pub fn as_integer(&self) -> Option<Integer> {
        match self {
            TyKind::Int(int_ty) => Some(Integer::Int(*int_ty)),
            TyKind::Uint(uint_ty) => Some(Integer::Uint(*uint_ty)),
            _ => None,
        }
    }

    #[must_use]
    pub fn is_integer(&self) -> bool {
        matches!(self, TyKind::Int(_) | TyKind::Uint(_))
    }

    /// Returns `true` if the ty kind is [`Infer`].
    ///
    /// [`Infer`]: TyKind::Infer
    #[must_use]
    pub fn is_infer(&self) -> bool {
        matches!(self, Self::Infer(..))
    }
}

impl Display for TyKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let e = match self {
            TyKind::Int(int_ty) => match int_ty {
                IntTy::I8 => "i8",
                IntTy::I16 => "i16",
                IntTy::I32 => "i32",
                IntTy::I64 => "i64",
            },
            TyKind::Uint(uint_ty) => match uint_ty {
                UintTy::U8 => "u8",
                UintTy::U16 => "u16",
                UintTy::U32 => "u32",
                UintTy::U64 => "u64",
            },
            TyKind::Float(float_ty) => match float_ty {
                FloatTy::F32 => "f32",
                FloatTy::F64 => "f64",
            },
            TyKind::String => "string",
            TyKind::Bytes => "bytes",
            TyKind::Bool => "bool",
            TyKind::Duration => "duration",
            TyKind::Time => "time",
            TyKind::Unit => "()",
            // Format array types as [element_type]
            TyKind::Array { inner } => return write!(f, "[{inner}]"),
            // Format function types as (arg1, arg2, ...) -> return_type
            TyKind::Function {
                arg,
                ret,
                is_procedure,
            } => {
                let arg_strs: Vec<String> = arg.iter().map(|arg| arg.to_string()).collect();
                return if is_procedure.is_some() {
                    write!(f, "prc ({}) -> {ret}", arg_strs.join(", "),)
                } else {
                    write!(f, "({}) -> {ret}", arg_strs.join(", "),)
                };
            }
            // Display inference variables with distinctive prefixes
            TyKind::Infer(infer_ty) => match infer_ty {
                InferTy::TyVar(var) => return write!(f, "{var}"),
                InferTy::IntVar(var) => return write!(f, "{var}"),
                InferTy::FloatVar(var) => return write!(f, "{var}"),
            },
            TyKind::External { path, ty } => return write!(f, "(extern<`{path}`>: {ty})"),
        };
        write!(f, "{e}")
    }
}
