use super::*;

/// Creates the builtin module containing all primitive types.
///
/// The builtin module provides access to fundamental types like integers, strings,
/// and other primitives that are available in all contexts without explicit imports.
pub fn create_builtin_module<'cx>(cx: &'cx TypingContext<'cx>) -> &'cx Module<'cx> {
    let mut builtin = Module::new(cx.alloc_toplevel_ident("builtin"));

    // Add all builtin primitive types
    builtin.add_type(cx.alloc_toplevel_ident("i8"), Ty::mk_i8(cx));
    builtin.add_type(cx.alloc_toplevel_ident("i16"), Ty::mk_i16(cx));
    builtin.add_type(cx.alloc_toplevel_ident("i32"), Ty::mk_i32(cx));
    builtin.add_type(cx.alloc_toplevel_ident("i64"), Ty::mk_i64(cx));

    builtin.add_type(cx.alloc_toplevel_ident("u8"), Ty::mk_u8(cx));
    builtin.add_type(cx.alloc_toplevel_ident("u16"), Ty::mk_u16(cx));
    builtin.add_type(cx.alloc_toplevel_ident("u32"), Ty::mk_u32(cx));
    builtin.add_type(cx.alloc_toplevel_ident("u64"), Ty::mk_u64(cx));

    builtin.add_type(cx.alloc_toplevel_ident("f32"), Ty::mk_f32(cx));
    builtin.add_type(cx.alloc_toplevel_ident("f64"), Ty::mk_f64(cx));

    builtin.add_type(cx.alloc_toplevel_ident("string"), Ty::mk_string(cx));
    builtin.add_type(cx.alloc_toplevel_ident("bool"), Ty::mk_bool(cx));
    builtin.add_type(cx.alloc_toplevel_ident("duration"), Ty::mk_duration(cx));
    builtin.add_type(cx.alloc_toplevel_ident("time"), Ty::mk_time(cx));

    builtin.add_library_function(
        cx.alloc_toplevel_ident("assert"),
        PolyTy::mono(Ty::mk_function(cx, vec![Ty::mk_bool(cx)], Ty::mk_unit(cx))),
    );

    builtin.add_library_function(cx.alloc_toplevel_ident("assert_eq"), {
        let var = Ty::mk_fresh(cx);
        let body = Ty::mk_function(cx, vec![var, var], Ty::mk_unit(cx));
        generalize_ty(body)
    });
    // any type can be printed, for now.
    builtin.add_library_function(
        cx.alloc_toplevel_ident("print"),
        generalize_ty(Ty::mk_function(cx, vec![Ty::mk_fresh(cx)], Ty::mk_unit(cx))),
    );

    builtin.add_constant(cx.alloc_toplevel_ident("true"), Ty::mk_bool(cx));
    builtin.add_constant(cx.alloc_toplevel_ident("false"), Ty::mk_bool(cx));

    cx.alloc_module(builtin)
}
