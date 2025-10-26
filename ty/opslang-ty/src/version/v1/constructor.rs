use super::*;

impl<'cx> Ty<'cx> {
    pub fn from_kind(cx: &'cx TypingContext<'cx>, kind: TyKind<'cx>) -> Self {
        cx.alloc_type(kind)
    }

    pub fn mk_int(cx: &'cx TypingContext<'cx>, int_ty: IntTy) -> Self {
        Self::from_kind(cx, TyKind::Int(int_ty))
    }

    pub fn mk_uint(cx: &'cx TypingContext<'cx>, uint_ty: UintTy) -> Self {
        Self::from_kind(cx, TyKind::Uint(uint_ty))
    }

    pub fn mk_float(cx: &'cx TypingContext<'cx>, float_ty: FloatTy) -> Self {
        Self::from_kind(cx, TyKind::Float(float_ty))
    }

    pub fn mk_i8(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_int(cx, IntTy::I8)
    }

    pub fn mk_i16(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_int(cx, IntTy::I16)
    }

    pub fn mk_i32(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_int(cx, IntTy::I32)
    }

    pub fn mk_i64(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_int(cx, IntTy::I64)
    }

    pub fn mk_u8(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_uint(cx, UintTy::U8)
    }

    pub fn mk_u16(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_uint(cx, UintTy::U16)
    }

    pub fn mk_u32(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_uint(cx, UintTy::U32)
    }

    pub fn mk_u64(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_uint(cx, UintTy::U64)
    }

    pub fn mk_f32(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_float(cx, FloatTy::F32)
    }

    pub fn mk_f64(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_float(cx, FloatTy::F64)
    }

    pub fn mk_string(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::String)
    }

    pub fn mk_bytes(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Bytes)
    }

    pub fn mk_bool(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Bool)
    }

    pub fn mk_duration(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Duration)
    }

    pub fn mk_time(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Time)
    }

    pub fn mk_array(cx: &'cx TypingContext<'cx>, inner: Ty<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Array { inner })
    }

    pub fn mk_procedure(
        cx: &'cx TypingContext<'cx>,
        procedure: Option<Procedure<'cx>>,
        arg: Vec<Ty<'cx>>,
        ret: Ty<'cx>,
    ) -> Self {
        Self::from_kind(
            cx,
            TyKind::Function {
                arg,
                ret,
                is_procedure: procedure,
            },
        )
    }

    pub fn mk_function(cx: &'cx TypingContext<'cx>, arg: Vec<Ty<'cx>>, ret: Ty<'cx>) -> Self {
        Self::from_kind(
            cx,
            TyKind::Function {
                arg,
                ret,
                is_procedure: None,
            },
        )
    }

    pub fn mk_variable(cx: &'cx TypingContext<'cx>, var: TyVid) -> Self {
        Self::from_kind(cx, TyKind::Infer(InferTy::TyVar(var)))
    }

    pub fn mk_fresh(cx: &'cx TypingContext<'cx>) -> Self {
        Self::mk_variable(cx, TyVid::fresh())
    }

    pub fn mk_int_var(cx: &'cx TypingContext<'cx>, var: IntVid) -> Self {
        Self::from_kind(cx, TyKind::Infer(InferTy::IntVar(var)))
    }

    pub fn mk_float_var(cx: &'cx TypingContext<'cx>, var: FloatVid) -> Self {
        Self::from_kind(cx, TyKind::Infer(InferTy::FloatVar(var)))
    }

    pub fn mk_unit(cx: &'cx TypingContext<'cx>) -> Self {
        Self::from_kind(cx, TyKind::Unit)
    }

    pub fn mk_external(cx: &'cx TypingContext<'cx>, path: ast::Path<'cx>, ty: Ty<'cx>) -> Self {
        Self::from_kind(cx, TyKind::External { path, ty })
    }
}
