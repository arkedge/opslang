use super::*;

/// Represents a polymorphic type with type variables.
///
/// A polymorphic type consists of type variables (TyVar) that can be instantiated
/// to create monomorphic types. This enables generic functions and types.
#[derive(Debug, Clone, PartialEq, Visit)]
pub struct PolyTy<'cx> {
    /// The type variables bound by this polymorphic type.
    pub type_vars: Vec<TyVid>,
    /// The body type that may contain the bound type variables.
    pub body: Ty<'cx>,
}

impl<'cx> PolyTy<'cx> {
    /// Creates a new polymorphic type.
    pub fn new(type_vars: Vec<TyVid>, body: Ty<'cx>) -> Self {
        Self { type_vars, body }
    }

    /// Creates a monomorphic type (no type variables).
    pub fn mono(body: Ty<'cx>) -> Self {
        Self {
            type_vars: Vec::new(),
            body,
        }
    }

    /// Instantiates the polymorphic type with fresh type variables.
    ///
    /// This creates a new monomorphic type by replacing bound type variables
    /// with fresh type variables for inference.
    pub fn instantiate(&self, cx: &'cx TypingContext<'cx>) -> Ty<'cx> {
        if self.type_vars.is_empty() {
            return self.body;
        }

        let mut substitution = hm::Substitution::new();
        for &var in &self.type_vars {
            let fresh_var = TyVid::fresh();
            let fresh_ty = Ty::mk_variable(cx, fresh_var);
            substitution.insert(var, fresh_ty);
        }
        substitution.apply_substitution_pure(cx, self.body)
    }
}
