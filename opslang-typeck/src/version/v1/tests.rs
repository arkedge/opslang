use opslang_ir::version::ResolvedItem;
use opslang_ty::version::{IntTy, ModuleItemDef};

use super::*;

fn parse_ident<'cx>(cx: &'cx ast::context::Context<'cx>, str: &str) -> ast::Path<'cx> {
    ast::Path::single(
        cx,
        str,
        ast::Span {
            start: ast::BytePos(0),
            end: ast::BytePos(0),
        },
    )
}

#[test]
fn test_display() {
    let cx = TypingContext::new();

    let int_type = Ty::mk_i32(&cx);
    let array_type = Ty::mk_array(&cx, int_type);

    assert_eq!(int_type.to_string(), "i32");
    assert_eq!(array_type.to_string(), "[i32]");
}

#[test]
fn test_builtin_module() {
    let cx = TypingContext::new();
    let ast_cx = ast::context::Context::new();
    let builtin = create_builtin_module(&cx);

    assert_eq!(builtin.name(), "builtin");
    assert!(builtin.lookup_item(parse_ident(&ast_cx, "i32")).is_some());
    assert!(builtin.lookup_item(parse_ident(&ast_cx, "f64")).is_some());
    assert!(
        builtin
            .lookup_item(parse_ident(&ast_cx, "unknown"))
            .is_none()
    );

    if let Some(ModuleItemDef::Type { id, ty }) = builtin.lookup_item(parse_ident(&ast_cx, "i32")) {
        assert_eq!(id.name, "i32");
        assert!(matches!(ty.kind(), TyKind::Int(IntTy::I32)));
    }
}

#[test]
fn test_module_loader() {
    let cx = TypingContext::new();
    let ast_cx = ast::context::Context::new();
    let builtin = create_builtin_module(&cx);

    let mut loader = ModuleLoader::new();
    loader.add_module(builtin);

    assert!(loader.lookup_module("builtin").is_some());
    assert!(loader.lookup_module("unknown").is_none());

    assert!(loader.resolve_path(parse_ident(&ast_cx, "i32")).is_some());
    assert!(
        loader
            .resolve_path(parse_ident(&ast_cx, "unknown"))
            .is_none()
    );
}

#[test]
fn test_type_checker_with_modules() {
    let cx = TypingContext::new();
    let ast_cx = ast::context::Context::new();
    let ir_cx = ir::Context::new();
    let builtin = create_builtin_module(&cx);

    let mut loader = ModuleLoader::new();
    loader.add_module(builtin);

    let checker = TypeChecker::with_module_loader(loader, &cx, &ir_cx);

    let i32_type = checker
        .resolve_type_from_path(parse_ident(&ast_cx, "i32"))
        .unwrap();
    assert!(matches!(i32_type.kind(), TyKind::Int(IntTy::I32)));

    let unknown_result = checker.resolve_type_from_path(parse_ident(&ast_cx, "unknown"));
    assert!(unknown_result.is_err());
}

#[test]
fn test_substitution() {
    let cx = TypingContext::new();
    let var = TyVid::fresh();
    let int_type = Ty::mk_i32(&cx);

    let mut subst = Substitution::new();
    subst.insert(var, int_type);

    let var_type = Ty::mk_variable(&cx, var);
    let result = subst.apply_substitution_pure(&cx, var_type);

    assert!(matches!(result.kind(), TyKind::Int(IntTy::I32)));
}

#[test]
fn test_unify_basic() {
    let cx = TypingContext::new();
    let ir_cx = ir::Context::new();

    let int_type1 = Ty::mk_i32(&cx);
    let int_type2 = Ty::mk_i32(&cx);
    let float_type = Ty::mk_f64(&cx);

    let chk = TypeChecker::new(&cx, &ir_cx);

    let result = chk.unify_pure(int_type1, int_type2);
    assert!(result.is_ok());
    assert!(result.unwrap().is_empty());

    let result = chk.unify_pure(int_type1, float_type);
    assert!(result.is_err());
}

#[test]
fn test_variable_resolution_priority() {
    let cx = TypingContext::new();
    let ir_cx = ir::Context::new();

    // these variables are defined here so as to live longer than the checker
    let i32_ident = ast::Ident {
        raw: "i32",
        span: ast::Span {
            start: ast::BytePos(0),
            end: ast::BytePos(0),
        },
    };
    let binding = [i32_ident];
    let path = ast::Path { segments: &binding };

    let builtin = create_builtin_module(&cx);

    let mut loader = ModuleLoader::new();
    loader.add_module(builtin);
    let mut checker = TypeChecker::with_module_loader(loader, &cx, &ir_cx);

    let mut env = Environment::<'_, '_>::new();

    // define local variable `i32` with type `String`
    let local_i32_type = Ty::mk_string(&cx);
    checker.bind(&mut env, i32_ident, local_i32_type);

    // resolve path `i32`
    let (resolved_type, _) = checker.typeck_path(&env, &path).unwrap();

    // should resolve to local variable type `String` instead of builtin type `i32`
    assert!(matches!(
        resolved_type.item,
        ResolvedItem::LocalVariable { .. }
    ));
}
