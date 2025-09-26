use std::collections::HashMap;

use super::{FloatTy, FloatVid, InferTy, IntTy, IntVid, Ty, TyKind, TyVid, TypingContext, UintTy};

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
            | TyKind::Unit
            | TyKind::External { .. } => ty,
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
