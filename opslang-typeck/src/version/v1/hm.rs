use anyhow::anyhow;

use super::{HashMap, Ty, TyKind, TyVid, TypingContext};
use opslang_ty::version::v1::{FloatTy, FloatVid, InferTy, IntTy, IntVid, UintTy};

/// Represents a type substitution mapping type variables to concrete types.
///
/// Substitutions are the result of unification operations and are used to
/// replace type variables with their inferred concrete types throughout the type system.
#[derive(Debug, Clone, Default)]
pub struct Substitution<'cx> {
    /// Maps type variables to their substituted types
    ty_map: HashMap<TyVid, Ty<'cx>>,
    /// Maps integer variables to their substituted types
    int_map: HashMap<IntVid, IntVarValue>,
    /// Maps float variables to their substituted types
    float_map: HashMap<FloatVid, FloatTy>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntVarValue {
    IntType(IntTy),
    UintType(UintTy),
}

impl<'cx> Substitution<'cx> {
    /// Creates a new empty substitution.
    ///
    /// An empty substitution represents the identity mapping where no variables are substituted.
    pub fn new() -> Self {
        Self {
            ty_map: HashMap::new(),
            int_map: HashMap::new(),
            float_map: HashMap::new(),
        }
    }

    /// Inserts a mapping from a type variable to a concrete type.
    ///
    /// This adds or replaces the substitution for the given type variable.
    pub fn insert(&mut self, var: TyVid, ty: Ty<'cx>) {
        self.ty_map.insert(var, ty);
    }

    /// Inserts a mapping from an integer variable to a concrete integer type.
    pub fn resolve_int_var(&mut self, var: IntVid, ty: IntVarValue) {
        self.int_map.insert(var, ty);
    }

    /// Inserts a mapping from an integer variable to a concrete integer type.
    pub fn resolve_int(&mut self, var: IntVid, ty: IntTy) {
        self.int_map.insert(var, IntVarValue::IntType(ty));
    }

    /// Inserts a mapping from an integer variable to a concrete unsigned integer type.
    pub fn resolve_uint(&mut self, var: IntVid, ty: UintTy) {
        self.int_map.insert(var, IntVarValue::UintType(ty));
    }

    /// Inserts a mapping from a float variable to a concrete float type.
    pub fn resolve_float(&mut self, var: FloatVid, ty: FloatTy) {
        self.float_map.insert(var, ty);
    }

    /// Gets the substituted type for a given type variable.
    ///
    /// Returns None if no substitution exists for the variable.
    pub fn get(&self, var: &TyVid) -> Option<Ty<'cx>> {
        self.ty_map.get(var).copied()
    }

    /// Gets the substituted integer type for a given integer variable.
    pub fn get_int(&self, var: &IntVid) -> Option<IntVarValue> {
        self.int_map.get(var).copied()
    }

    /// Gets the substituted float type for a given float variable.
    pub fn get_float(&self, var: &FloatVid) -> Option<FloatTy> {
        self.float_map.get(var).copied()
    }

    /// Checks if this substitution is empty (contains no mappings).
    ///
    /// An empty substitution is equivalent to the identity substitution.
    pub fn is_empty(&self) -> bool {
        self.ty_map.is_empty() && self.int_map.is_empty() && self.float_map.is_empty()
    }

    /// Applies this substitution to a type, replacing type variables with their substituted types.
    ///
    /// This recursively walks through the type structure and applies substitutions to all
    /// type variables found. The process continues until no more substitutions can be applied.
    pub fn apply_substitution(&self, cx: &'cx TypingContext<'cx>, ty: &mut Ty<'cx>) {
        *ty = self.apply_substitution_pure(cx, *ty);
    }

    pub fn apply_substitution_pure(&self, cx: &'cx TypingContext<'cx>, ty: Ty<'cx>) -> Ty<'cx> {
        match ty.kind() {
            TyKind::Infer(InferTy::TyVar(var)) => {
                // Recursively apply substitutions to handle chains of substitutions
                if let Some(substituted) = self.ty_map.get(var) {
                    self.apply_substitution_pure(cx, *substituted)
                } else {
                    ty
                }
            }
            TyKind::Infer(InferTy::IntVar(var)) => {
                if let Some(substituted_ty) = self.int_map.get(var) {
                    match substituted_ty {
                        IntVarValue::IntType(int_ty) => Ty::mk_int(cx, *int_ty),
                        IntVarValue::UintType(uint_ty) => Ty::mk_uint(cx, *uint_ty),
                    }
                } else {
                    ty
                }
            }
            TyKind::Infer(InferTy::FloatVar(var)) => {
                if let Some(substituted_ty) = self.float_map.get(var) {
                    Ty::mk_float(cx, *substituted_ty)
                } else {
                    ty
                }
            }
            TyKind::Array { inner } => {
                let substituted_inner = self.apply_substitution_pure(cx, *inner);
                // Only allocate a new type if something actually changed
                if substituted_inner == *inner {
                    ty
                } else {
                    cx.alloc_type(TyKind::Array {
                        inner: substituted_inner,
                    })
                }
            }
            TyKind::Function { arg: args, ret } => {
                let mut changed = false;
                // Apply substitutions to all argument types
                let substituted_args: Vec<Ty<'cx>> = args
                    .iter()
                    .map(|&arg| {
                        let substituted = self.apply_substitution_pure(cx, arg);
                        if substituted != arg {
                            changed = true;
                        }
                        substituted
                    })
                    .collect();
                // Apply substitution to return type
                let substituted_ret = self.apply_substitution_pure(cx, *ret);
                if substituted_ret != *ret {
                    changed = true;
                }

                // Only allocate new function type if something changed
                if changed {
                    cx.alloc_type(TyKind::Function {
                        arg: substituted_args,
                        ret: substituted_ret,
                    })
                } else {
                    ty
                }
            }
            TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Float(_)
            | TyKind::String
            | TyKind::Bytes
            | TyKind::Bool
            | TyKind::Duration
            | TyKind::Time
            | TyKind::Unit => ty,
        }
    }

    /// Composes this substitution with another substitution.
    ///
    /// The composition applies the first substitution to the types in the second substitution,
    /// then combines both mappings. This ensures that variable chains are properly resolved.
    pub fn compose(&mut self, other: &Substitution<'cx>, cx: &'cx TypingContext<'cx>) {
        if self.is_empty() {
            self.clone_from(other);
            return;
        }
        // Apply this substitution to all types in the other substitution
        for (var, &ty) in &other.ty_map {
            let substituted_ty = self.apply_substitution_pure(cx, ty);
            self.insert(*var, substituted_ty);
        }

        // Directly extend integer and float substitutions
        self.int_map.extend(other.int_map.iter());
        self.float_map.extend(other.float_map.iter());
    }
}

impl<'cx> super::TypeChecker<'cx> {
    /// Attempts to unify two types, producing a substitution that makes them equal.
    ///
    /// Unification is the core algorithm for type inference, determining what type variables
    /// must be bound to make two types compatible. This implements the standard unification
    /// algorithm with occurs check to prevent infinite types.
    pub fn unify(
        &self,
        subst: &mut Substitution<'cx>,
        t1: Ty<'cx>,
        t2: Ty<'cx>,
    ) -> super::Result<()> {
        match (t1.kind(), t2.kind()) {
            // Inference variables
            (TyKind::Infer(InferTy::TyVar(var1)), TyKind::Infer(InferTy::TyVar(var2)))
                if var1 == var2 =>
            {
                Ok(())
            }
            (TyKind::Infer(InferTy::IntVar(var1)), TyKind::Infer(InferTy::IntVar(var2)))
                if var1 == var2 =>
            {
                Ok(())
            }
            (TyKind::Infer(InferTy::FloatVar(var1)), TyKind::Infer(InferTy::FloatVar(var2)))
                if var1 == var2 =>
            {
                Ok(())
            }

            // General type variable unification
            (TyKind::Infer(InferTy::TyVar(var)), ty) | (ty, TyKind::Infer(InferTy::TyVar(var))) => {
                if ty.occurs(*var) {
                    Err(anyhow!("occurs check failed: {var} occurs in {ty}",))
                } else {
                    subst.insert(*var, Ty(ty));
                    Ok(())
                }
            }

            // Integer variable unification - can unify with any integer or unsigned type
            (TyKind::Infer(InferTy::IntVar(var)), TyKind::Int(int_ty))
            | (TyKind::Int(int_ty), TyKind::Infer(InferTy::IntVar(var))) => {
                subst.resolve_int(*var, *int_ty);
                Ok(())
            }
            (TyKind::Infer(InferTy::IntVar(var)), TyKind::Uint(uint_ty))
            | (TyKind::Uint(uint_ty), TyKind::Infer(InferTy::IntVar(var))) => {
                subst.resolve_uint(*var, *uint_ty);
                Ok(())
            }

            // Integer variable unification - can unify with any integer or unsigned type
            (TyKind::Infer(InferTy::IntVar(var1)), TyKind::Infer(InferTy::IntVar(var2))) => {
                let val1 = subst.get_int(var1);
                let val2 = subst.get_int(var2);
                match (val1, val2) {
                    (None, None) => {
                        // ok, do nothing
                        Ok(())
                    }
                    (None, Some(concrete)) => {
                        subst.resolve_int_var(*var1, concrete);
                        Ok(())
                    }
                    (Some(concrete), None) => {
                        subst.resolve_int_var(*var2, concrete);
                        Ok(())
                    }
                    (Some(concrete1), Some(concrete2)) => {
                        if concrete1 != concrete2 {
                            Err(anyhow!("failed to unify {concrete1:?} and {concrete2:?}",))
                        } else {
                            Ok(())
                        }
                    }
                }
            }

            // Float variable unification - can unify with any float type
            (TyKind::Infer(InferTy::FloatVar(var)), TyKind::Float(float_ty))
            | (TyKind::Float(float_ty), TyKind::Infer(InferTy::FloatVar(var))) => {
                subst.resolve_float(*var, *float_ty);
                Ok(())
            }
            // Primitive types unify only with themselves
            (TyKind::Int(int1), TyKind::Int(int2)) if int1 == int2 => Ok(()),
            (TyKind::Uint(uint1), TyKind::Uint(uint2)) if uint1 == uint2 => Ok(()),
            (TyKind::Float(float1), TyKind::Float(float2)) if float1 == float2 => Ok(()),
            (TyKind::String, TyKind::String)
            | (TyKind::Bool, TyKind::Bool)
            | (TyKind::Duration, TyKind::Duration)
            | (TyKind::Time, TyKind::Time)
            | (TyKind::Unit, TyKind::Unit) => Ok(()),
            // Array types unify if their element types unify
            (TyKind::Array { inner: inner1 }, TyKind::Array { inner: inner2 }) => {
                self.unify(subst, *inner1, *inner2)
            }
            // Function types unify if they have the same arity and corresponding types unify
            (
                TyKind::Function {
                    arg: args1,
                    ret: ret1,
                },
                TyKind::Function {
                    arg: args2,
                    ret: ret2,
                },
            ) => {
                if args1.len() != args2.len() {
                    return Err(anyhow!(
                        "function arity mismatch: {} vs {}",
                        args1.len(),
                        args2.len()
                    ));
                }

                // Unify corresponding argument types
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    self.unify(subst, *arg1, *arg2)?;
                }

                // Apply accumulated substitutions to return types before unifying
                let substituted_ret1 = subst.apply_substitution_pure(self.tcx, *ret1);
                let substituted_ret2 = subst.apply_substitution_pure(self.tcx, *ret2);
                self.unify(subst, substituted_ret1, substituted_ret2)?;
                Ok(())
            }
            // All other combinations are incompatible
            _ => Err(anyhow!("cannot unify {t1} and {t2}",)),
        }
    }

    pub fn unify_pure(&self, t1: Ty<'cx>, t2: Ty<'cx>) -> super::Result<Substitution<'cx>> {
        let mut subst = Substitution::new();
        self.unify(&mut subst, t1, t2)?;
        Ok(subst)
    }
}
