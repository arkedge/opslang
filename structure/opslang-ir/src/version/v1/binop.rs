use super::*;

/// IR-specific binary operation that replaces AST BinOp.
///
/// Unlike AST BinOp which only has token information, IR BinOp contains
/// resolved type information for builtin operations.
///
/// FIXME: consider adding these variants into TypeFamily directly to avoid copying
/// structure.
#[derive(Debug, PartialEq, Clone, Copy, Visit)]
#[skip_all_visit]
pub enum BinOp<'cx> {
    /// Logical AND operation (unchanged from AST)
    And {
        span: token::AndAnd<'cx, IrTypeFamily>,
    },
    /// Logical OR operation (unchanged from AST)
    Or {
        span: token::OrOr<'cx, IrTypeFamily>,
    },
    /// Multiplication operation (unchanged from AST)
    Mul {
        kind: BuiltinMul,
        span: token::Star<'cx, IrTypeFamily>,
    },
    /// Division operation (unchanged from AST)
    Div {
        kind: BuiltinDiv,
        span: token::Slash<'cx, IrTypeFamily>,
    },
    /// Modulo operation (unchanged from AST)
    Mod {
        span: token::Percent<'cx, IrTypeFamily>,
    },

    /// Builtin addition with resolved types
    Add {
        kind: BuiltinAdd,
        span: token::Plus<'cx, IrTypeFamily>,
    },
    /// Builtin subtraction with resolved types
    Sub {
        kind: BuiltinSub,
        span: token::Hyphen<'cx, IrTypeFamily>,
    },
}

impl<'cx> BinOp<'cx> {
    pub fn result_ty(&self, cx: &'cx TypingContext<'cx>) -> Ty<'cx> {
        match self {
            BinOp::And { .. } | BinOp::Or { .. } => Ty::mk_bool(cx),
            BinOp::Mul { kind, .. } => kind.result_ty(cx),
            BinOp::Div { kind, .. } => kind.result_ty(cx),
            BinOp::Mod { .. } => Ty::mk_i64(cx),
            BinOp::Add { kind, .. } => kind.result_ty(cx),
            BinOp::Sub { kind, .. } => kind.result_ty(cx),
        }
    }
}

impl<'cx> Typed<'cx> for BinOp<'cx> {
    type Ty = Ty<'cx>;

    fn ty(&self, cx: &'cx TypingContext<'cx>) -> Self::Ty {
        self.result_ty(cx)
    }
}

/// Represents the operand types supported by builtin add and subtract operations.
///
/// This enum defines the valid type combinations for addition and subtraction operations,
/// including integer + integer, float + float, Duration + Duration, and datetime arithmetic.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BuiltinAddSubOperand {
    /// Integer addition/subtraction: integer + integer
    Int(MagmaTriad<ty::IntTy>),
    /// Unsigned integer + Unsigned integer
    Uint(MagmaTriad<ty::UintTy>),
    /// Float addition/subtraction: float + float
    /// Mixed FloatTy are permitted, output type uses larger bit width
    Float(MagmaTriad<ty::FloatTy>),
    /// Duration addition/subtraction: Duration + Duration
    DurationDuration,
    /// DateTime + Duration
    DateTimeDuration,
    /// Duration + DateTime
    DurationDateTime,
}

impl BuiltinAddSubOperand {
    pub fn result_ty<'cx>(&self, cx: &'cx TypingContext<'cx>) -> Ty<'cx> {
        match self {
            BuiltinAddSubOperand::Int(triad) => Ty::mk_int(cx, triad.result_ty),
            BuiltinAddSubOperand::Uint(triad) => Ty::mk_uint(cx, triad.result_ty),
            BuiltinAddSubOperand::Float(triad) => Ty::mk_float(cx, triad.result_ty),
            BuiltinAddSubOperand::DurationDuration => Ty::mk_duration(cx),
            BuiltinAddSubOperand::DateTimeDuration => Ty::mk_time(cx),
            BuiltinAddSubOperand::DurationDateTime => Ty::mk_time(cx),
        }
    }
}

/// Builtin add operation with type information.
///
/// This represents a resolved addition operation with specific operand types
/// determined during type checking.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BuiltinAdd {
    /// Basic add operation with operand type information
    Basic(BuiltinAddSubOperand),
}

impl BuiltinAdd {
    pub fn result_ty<'cx>(&self, cx: &'cx TypingContext<'cx>) -> Ty<'cx> {
        match self {
            BuiltinAdd::Basic(operand) => operand.result_ty(cx),
        }
    }
}

/// Builtin subtract operation with type information.
///
/// This represents a resolved subtraction operation with specific operand types
/// determined during type checking.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BuiltinSub {
    /// Basic subtract operation with operand type information
    Basic(BuiltinAddSubOperand),
}

impl BuiltinSub {
    pub fn result_ty<'cx>(&self, cx: &'cx TypingContext<'cx>) -> Ty<'cx> {
        match self {
            BuiltinSub::Basic(operand) => operand.result_ty(cx),
        }
    }
}

/// Represents the operand types supported by builtin multiplication and division operations.
///
/// This enum defines the valid type combinations for multiplication and division operations,
/// including integer * integer, float * float, and duration * integer.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BuiltinMulDivOperand {
    /// Integer addition/subtraction: integer * integer.
    Int(MagmaTriad<ty::IntTy>),
    /// Unsigned integer * unsigned integer.
    Uint(MagmaTriad<ty::UintTy>),
    /// Float multiplication/division: float * float.
    ///
    /// Mixed FloatTy are permitted, output type uses larger bit width.
    Float(MagmaTriad<ty::FloatTy>),
    /// Duration * numeric.
    DurationNumeric(ty::Numeric),
}

impl BuiltinMulDivOperand {
    pub fn result_ty<'cx>(&self, cx: &'cx TypingContext<'cx>) -> Ty<'cx> {
        match self {
            BuiltinMulDivOperand::Int(triad) => Ty::mk_int(cx, triad.result_ty),
            BuiltinMulDivOperand::Uint(triad) => Ty::mk_uint(cx, triad.result_ty),
            BuiltinMulDivOperand::Float(triad) => Ty::mk_float(cx, triad.result_ty),
            BuiltinMulDivOperand::DurationNumeric(_) => Ty::mk_duration(cx),
        }
    }
}

/// Builtin multiplication operation with type information.
///
/// This represents a resolved multiplication operation with specific operand types
/// determined during type checking.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BuiltinMul {
    /// Basic multiply operation with operand type information.
    Basic(BuiltinMulDivOperand),
}

impl BuiltinMul {
    pub fn result_ty<'cx>(&self, cx: &'cx TypingContext<'cx>) -> Ty<'cx> {
        match self {
            BuiltinMul::Basic(operand) => operand.result_ty(cx),
        }
    }
}

/// Builtin division operation with type information.
///
/// This represents a resolved division operation with specific operand types
/// determined during type checking.
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BuiltinDiv {
    /// Basic divide operation with operand type information.
    Basic(BuiltinMulDivOperand),
}

impl BuiltinDiv {
    pub fn result_ty<'cx>(&self, cx: &'cx TypingContext<'cx>) -> Ty<'cx> {
        match self {
            BuiltinDiv::Basic(operand) => operand.result_ty(cx),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
/// Represents a triad of types for binary operations.
pub struct MagmaTriad<T> {
    pub lhs_ty: T,
    pub rhs_ty: T,
    pub result_ty: T,
}
