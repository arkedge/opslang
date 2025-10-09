use anyhow::anyhow;
use opslang_ty::version::v1::{InferTy, PolyTy, Substitution, Ty, TyKind, TyVid};
use std::collections::HashSet;

use super::*;

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
                    arg: arg1,
                    ret: ret1,
                    is_procedure: prc1,
                },
                TyKind::Function {
                    arg: arg2,
                    ret: ret2,
                    is_procedure: prc2,
                },
            ) => {
                if arg1.len() != arg2.len() {
                    return Err(anyhow!(
                        "function arity mismatch: {} vs {}",
                        arg1.len(),
                        arg2.len()
                    ));
                }
                if prc1.is_some() && prc2.is_some() && prc1 != prc2 {
                    return Err(anyhow!("procedure mismatch: {prc1:?} vs {prc2:?}"));
                }

                // Unify corresponding argument types
                for (arg1, arg2) in arg1.iter().zip(arg2.iter()) {
                    self.unify(subst, *arg1, *arg2)?;
                }

                // Apply accumulated substitutions to return types before unifying
                let substituted_ret1 = subst.apply_substitution_pure(self.tcx, *ret1);
                let substituted_ret2 = subst.apply_substitution_pure(self.tcx, *ret2);
                self.unify(subst, substituted_ret1, substituted_ret2)?;
                Ok(())
            }

            // All other combinations are incompatible

            // Below we list all the possible TyKind variants to ensure exhaustiveness.
            // If you add a new TyKind, you must handle it *above* and here.
            (TyKind::Int(_), _)
            | (TyKind::Uint(_), _)
            | (TyKind::Float(_), _)
            | (TyKind::String, _)
            | (TyKind::Bytes, _)
            | (TyKind::Bool, _)
            | (TyKind::Duration, _)
            | (TyKind::Time, _)
            | (TyKind::Array { .. }, _)
            | (TyKind::Function { .. }, _)
            | (TyKind::Infer(_), _)
            | (TyKind::Unit, _)
            | (TyKind::External { .. }, _) => Err(anyhow!("cannot unify {t1} and {t2}",)),
        }
    }

    pub fn unify_pure(&self, t1: Ty<'cx>, t2: Ty<'cx>) -> super::Result<Substitution<'cx>> {
        let mut subst = Substitution::new();
        self.unify(&mut subst, t1, t2)?;
        Ok(subst)
    }
}

/// Generalizes a type by quantifying over all free type variables.
///
/// This function collects all type variables that appear in the given type
/// and creates a polymorphic type that quantifies over them.
///
/// FIXME: This function can be defined in opslang-ty crate, but it would require
/// big changes to macro crates.
pub fn generalize_ty<'cx>(body: Ty<'cx>) -> PolyTy<'cx> {
    use opslang_visitor::Visitor;
    let mut visitor = TyVarCollector::new();
    visitor.visit(&body);
    PolyTy {
        type_vars: visitor.type_vars,
        body,
    }
}

/// A visitor that collects all type variables in a type.
struct TyVarCollector {
    type_vars: Vec<TyVid>,
    seen: HashSet<TyVid>,
}

impl TyVarCollector {
    fn new() -> Self {
        Self {
            type_vars: Vec::new(),
            seen: HashSet::new(),
        }
    }
}

opslang_ir_macro::v1_ir_visitor_impl!(for TyVarCollector {
    fn visit_ty_vid(&mut self, var: &TyVid) {
        if !self.seen.contains(var) {
            self.seen.insert(*var);
            self.type_vars.push(*var);
        }
    }
});

/// Mutable `Cow`, do not copy on write.
enum CowMut<'a, T> {
    Borrowed(&'a mut T),
    Owned(T),
}

impl<'a, T> Deref for CowMut<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            CowMut::Borrowed(r) => r,
            CowMut::Owned(v) => v,
        }
    }
}

impl<'a, T> DerefMut for CowMut<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            CowMut::Borrowed(r) => r,
            CowMut::Owned(v) => v,
        }
    }
}

/// Visitor for applying substitutions to all types in the IR.
pub struct SubstitutionVisitor<'cx, 'a> {
    ty_last_seen: Option<Ty<'cx>>,
    subst: CowMut<'a, Substitution<'cx>>,
    tcx: &'cx TypingContext<'cx>,
}

impl<'cx, 'a> SubstitutionVisitor<'cx, 'a> {
    pub fn new(subst: Substitution<'cx>, tcx: &'cx TypingContext<'cx>) -> Self {
        Self {
            subst: CowMut::Owned(subst),
            tcx,
            ty_last_seen: None,
        }
    }
    pub fn new_borrowed(subst: &'a mut Substitution<'cx>, tcx: &'cx TypingContext<'cx>) -> Self {
        Self {
            subst: CowMut::Borrowed(subst),
            tcx,
            ty_last_seen: None,
        }
    }
}

opslang_ir_macro::v1_ir_visitor_impl!(for SubstitutionVisitor<'cx, '_> {
    fn visit_expr_mut(&mut self, expr: &mut ir::Expr<'cx>) {
        use ir::IrMutVisitor;
        // visit ty first
        self.visit_ty_mut(&mut expr.ty);
        self.visit_mut(&mut expr.kind);
    }
    fn visit_ty_mut(&mut self, ty: &mut Ty<'cx>) {
        self.subst.apply_substitution(self.tcx, ty);
        let kind = ty.kind();
        match kind {
            TyKind::Infer(InferTy::IntVar(int_vid)) => {
                // Resolve integer type variables to i32 by default
                self.subst.resolve_int(*int_vid, IntTy::I64);
            }
            TyKind::Infer(InferTy::FloatVar(float_vid)) => {
                // Resolve float type variables to f64 by default in Rust
                self.subst.resolve_float(*float_vid, FloatTy::F64);
            }
            _ => {}
        }
        self.subst.apply_substitution(self.tcx, ty);
        self.ty_last_seen = Some(*ty);
    }
    fn visit_ty(&mut self, _ty: &Ty<'cx>) {
        dbg!(_ty);
        panic!("Found `Ty` immutability. this prevents type variable resolution. review changes to IR structure and eliminate possession of immutable `Ty`pes.");
    }
    fn visit_numeric_mut(&mut self, numeric: &mut ir::Numeric<'cx>) {
        if let ir::NumericKind::Repr(unparsed) = numeric.kind {
            let literal = typeck_literal::parse_literal(unparsed, self.ty_last_seen.unwrap()).unwrap();
            numeric.kind = literal;
        }
    }
    fn visit_resolved_item_mut(&mut self, _resolved_item: &mut ir::ResolvedItem<'cx>) {
        // `ResolvedItem` is immutable, not calling super
    }
});
