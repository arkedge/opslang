use super::*;

/// Represents inference variables used during type inference.
///
/// Different kinds of inference variables allow for more precise type inference,
/// particularly for numeric types that can have default fallbacks.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
pub enum InferTy {
    /// General type inference variable
    TyVar(TyVid),
    /// Integer inference variable that can fallback to default integer type
    IntVar(IntVid),
    /// Float inference variable that can fallback to default float type  
    FloatVar(FloatVid),
}

/// Integer type variable for type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub struct IntVid(u32);

impl IntVid {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel))
    }
}

impl std::fmt::Display for IntVid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "$int{}", self.0)
    }
}

/// Float type variable for type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub struct FloatVid(u32);

impl FloatVid {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel))
    }
}

impl std::fmt::Display for FloatVid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "$float{}", self.0)
    }
}

/// General type variable for type inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Visit)]
#[skip_all_visit]
pub struct TyVid(u32);

impl TyVid {
    pub fn fresh() -> Self {
        static COUNTER: AtomicU32 = AtomicU32::new(0);
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::AcqRel))
    }
}

impl std::fmt::Display for TyVid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "$t{}", self.0)
    }
}
