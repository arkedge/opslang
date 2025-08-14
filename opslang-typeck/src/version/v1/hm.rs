use anyhow::anyhow;

use super::{HashMap, Ty, TyKind, TypeVariable, TypingContext};

/// Represents a type substitution mapping type variables to concrete types.
///
/// Substitutions are the result of unification operations and are used to
/// replace type variables with their inferred concrete types throughout the type system.
#[derive(Debug, Clone, Default)]
pub struct Substitution<'cx> {
    /// Maps type variables to their substituted types
    map: HashMap<TypeVariable, Ty<'cx>>,
}

impl<'cx> Substitution<'cx> {
    /// Creates a new empty substitution.
    ///
    /// An empty substitution represents the identity mapping where no variables are substituted.
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Inserts a mapping from a type variable to a concrete type.
    ///
    /// This adds or replaces the substitution for the given type variable.
    pub fn insert(&mut self, var: TypeVariable, ty: Ty<'cx>) {
        self.map.insert(var, ty);
    }

    /// Gets the substituted type for a given type variable.
    ///
    /// Returns None if no substitution exists for the variable.
    pub fn get(&self, var: &TypeVariable) -> Option<Ty<'cx>> {
        self.map.get(var).copied()
    }

    /// Checks if this substitution is empty (contains no mappings).
    ///
    /// An empty substitution is equivalent to the identity substitution.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
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
            TyKind::Variable(var) => {
                // Recursively apply substitutions to handle chains of substitutions
                if let Some(substituted) = self.map.get(var) {
                    self.apply_substitution_pure(cx, *substituted)
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
            TyKind::Int
            | TyKind::Float
            | TyKind::String
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
        }
        // Apply this substitution to all types in the other substitution
        for (var, &ty) in &other.map {
            let substituted_ty = self.apply_substitution_pure(cx, ty);
            self.insert(*var, substituted_ty);
        }
    }
}

impl super::TypeChecker<'_> {
    /// Attempts to unify two types, producing a substitution that makes them equal.
    ///
    /// Unification is the core algorithm for type inference, determining what type variables
    /// must be bound to make two types compatible. This implements the standard unification
    /// algorithm with occurs check to prevent infinite types.
    pub fn unify<'a>(
        cx: &'a TypingContext<'a>,
        subst: &mut Substitution<'a>,
        t1: Ty<'a>,
        t2: Ty<'a>,
    ) -> super::Result<()> {
        match (t1.kind(), t2.kind()) {
            // Two identical type variables unify trivially
            (TyKind::Variable(var1), TyKind::Variable(var2)) if var1 == var2 => Ok(()),
            // Unify type variable with concrete type (occurs check prevents infinite types)
            (TyKind::Variable(var), ty) | (ty, TyKind::Variable(var)) => {
                if ty.occurs(*var) {
                    Err(anyhow!(
                        "Occurs check failed: {} occurs in {}",
                        var,
                        ty.display(cx)
                    ))
                } else {
                    subst.insert(*var, Ty(ty));
                    Ok(())
                }
            }
            // Primitive types unify only with themselves
            (TyKind::Int, TyKind::Int)
            | (TyKind::Float, TyKind::Float)
            | (TyKind::String, TyKind::String)
            | (TyKind::Bool, TyKind::Bool)
            | (TyKind::Duration, TyKind::Duration)
            | (TyKind::Time, TyKind::Time)
            | (TyKind::Unit, TyKind::Unit) => Ok(()),
            // Array types unify if their element types unify
            (TyKind::Array { inner: inner1 }, TyKind::Array { inner: inner2 }) => {
                Self::unify(cx, subst, *inner1, *inner2)
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
                        "Function arity mismatch: {} vs {}",
                        args1.len(),
                        args2.len()
                    ));
                }

                // Unify corresponding argument types
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    Self::unify(cx, subst, *arg1, *arg2)?;
                }

                // Apply accumulated substitutions to return types before unifying
                let substituted_ret1 = subst.apply_substitution_pure(cx, *ret1);
                let substituted_ret2 = subst.apply_substitution_pure(cx, *ret2);
                Self::unify(cx, subst, substituted_ret1, substituted_ret2)?;
                Ok(())
            }
            // All other combinations are incompatible
            _ => Err(anyhow!(
                "Cannot unify {} and {}",
                t1.display(cx),
                t2.display(cx)
            )),
        }
    }

    pub fn unify_pure<'a>(
        cx: &'a TypingContext<'a>,
        t1: Ty<'a>,
        t2: Ty<'a>,
    ) -> super::Result<Substitution<'a>> {
        let mut subst = Substitution::new();
        Self::unify(cx, &mut subst, t1, t2)?;
        Ok(subst)
    }
}
