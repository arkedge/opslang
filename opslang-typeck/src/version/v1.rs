use anyhow::anyhow;
use chrono::Utc;
use opslang_ast::v1::token::IntoToken;
use opslang_ast::v1::{self as ast};
use opslang_ir::version::v1::{self as ir};

use ir::Typed;
use opslang_ty::version::v1::{
    self as ty, FloatTy, Ident, InferTy, IntTy, Module, ModuleItem, ModuleLoader, Ty, TyKind,
    TyVid, TypingContext,
};
use opslang_visitor::VisitorMut;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

type Result<T, E = anyhow::Error> = std::result::Result<T, E>;

mod hm;
use hm::Substitution;
mod environment;
use environment::Environment;
mod lower;
mod typeck_apply;
mod typeck_binary;
mod typeck_block;
mod typeck_constant;
mod typeck_expr;
mod typeck_function;
mod typeck_literal;
mod typeck_statement;

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

    cx.alloc_module(builtin)
}

/// The main type checker that performs type inference and checking.
pub struct TypeChecker<'cx> {
    /// Module loader for resolving external symbols.
    module_loader: ModuleLoader<'cx>,

    /// Optional external resolver for custom type resolution.
    external_resolver: Option<Box<dyn ExternalResolver<'cx>>>,

    tcx: &'cx TypingContext<'cx>,
    ir_cx: &'cx ir::Context<'cx>,
}

impl core::fmt::Debug for TypeChecker<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        let TypeChecker { module_loader, .. } = self;
        f.debug_struct("TypeChecker")
            .field("module_loader", &module_loader)
            .finish()
    }
}

/// Trait for resolving external types by path.
///
/// Resolved item belongs to no module.
pub trait ExternalResolver<'cx> {
    /// Resolves a path to a type, returning `None` if not found.
    fn resolve(&self, path: ast::Path<'cx>, cx: &'cx TypingContext<'cx>) -> Option<Ty<'cx>>;
}

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

/// Visitor for applying substitutions to all types in the IR
struct SubstitutionVisitor<'cx, 'a> {
    ty_last_seen: Option<Ty<'cx>>,
    subst: CowMut<'a, Substitution<'cx>>,
    tcx: &'cx TypingContext<'cx>,
}

impl<'cx, 'a> SubstitutionVisitor<'cx, 'a> {
    fn new(subst: Substitution<'cx>, tcx: &'cx TypingContext<'cx>) -> Self {
        Self {
            subst: CowMut::Owned(subst),
            tcx,
            ty_last_seen: None,
        }
    }
    fn new_borrowed(subst: &'a mut Substitution<'cx>, tcx: &'cx TypingContext<'cx>) -> Self {
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

impl<'cx> TypeChecker<'cx> {
    /// Creates a new type checker with empty module loader.
    ///
    /// The type checker starts with no modules loaded.
    pub fn new(tcx: &'cx TypingContext<'cx>, ir_cx: &'cx ir::Context<'cx>) -> Self {
        Self {
            external_resolver: None,
            module_loader: ModuleLoader::new(),
            tcx,
            ir_cx,
        }
    }

    /// Creates a new type checker with the given module loader.
    ///
    /// This allows pre-loading modules before starting type checking operations.
    pub fn with_module_loader(
        module_loader: ModuleLoader<'cx>,
        tcx: &'cx TypingContext<'cx>,
        ir_cx: &'cx ir::Context<'cx>,
    ) -> Self {
        Self {
            external_resolver: None,
            module_loader,
            tcx,
            ir_cx,
        }
    }

    pub fn add_external_resolver(
        &mut self,
        resolver: impl ExternalResolver<'cx> + 'static,
    ) -> &mut Self {
        self.external_resolver = Some(Box::new(resolver));
        self
    }

    /// Adds a module to the type checker's module loader.
    ///
    /// This makes the module's exported items available for type resolution.
    pub fn add_module(&mut self, module: &'cx Module<'cx>) {
        self.module_loader.add_module(module);
    }

    /// Performs type checking on an AST program and converts it to IR.
    ///
    /// This is the main entry point for type checking and IR generation.
    /// It processes all top-level definitions, performs type inference,
    /// and converts the AST to a typed IR representation.
    pub fn typeck(&mut self, program: &ast::Program<'cx>) -> Result<ir::Program<'cx>> {
        // Create global environment for top-level definitions
        let mut global_env = Environment::<'cx, '_>::new();

        // First pass: collect all function and constant signatures
        for definition in program.toplevel_items {
            if let Some(kind) = &definition.kind {
                match kind {
                    ast::DefinitionKind::Function(func_def) => {
                        self.register_function_signature(&mut global_env, func_def)?;
                    }
                    ast::DefinitionKind::Constant(const_def) => {
                        self.register_constant_signature(&mut global_env, const_def)?;
                    }
                }
            }
        }

        // Second pass: type check implementations and process comments
        let mut ir_definitions = Vec::new();
        let mut pending_comments: Vec<&'cx ast::Comment<'cx>> = Vec::new();

        for toplevel_item in program.toplevel_items {
            if toplevel_item.is_empty() {
                // Empty item (empty line) - discard any pending comments
                pending_comments.clear();
                continue;
            }

            // Collect comment from this item if present
            if let Some(comment) = &toplevel_item.comment {
                pending_comments.push(comment);
            }

            // If this item has a definition, create an IR Definition
            if let Some(kind) = &toplevel_item.kind {
                let leading_comment = if pending_comments.is_empty() {
                    None
                } else {
                    Some(lower::merge_comments(self.ir_cx, &pending_comments)?)
                };

                let ir_kind = match kind {
                    ast::DefinitionKind::Function(func_def) => {
                        let ir_func = self.typeck_function(&global_env, func_def)?;
                        ir::DefinitionKind::Function(ir_func)
                    }
                    ast::DefinitionKind::Constant(const_def) => {
                        let ir_const = self.typeck_constant(&global_env, const_def)?;
                        ir::DefinitionKind::Constant(ir_const)
                    }
                };

                let ir_definition = ir::Definition {
                    comment_before: leading_comment,
                    comment_trailing: None, // TODO: Handle trailing comments
                    kind: ir_kind,
                };

                ir_definitions.push(ir_definition);
                pending_comments.clear();
            }
        }

        Ok(ir::Program {
            toplevel_items: self.ir_cx.alloc_definition_slice(ir_definitions),
        })
    }

    fn register_function_signature(
        &mut self,
        env: &mut Environment<'cx, '_>,
        func_def: &ast::FunctionDef<'cx>,
    ) -> Result<()> {
        let func_name = func_def.name;

        // Check if function with same name already exists
        if env.lookup_name(func_name).is_some() {
            return Err(anyhow!("function '{func_name}' is already defined"));
        }

        let mut param_types = Vec::new();

        for param in func_def.parameters {
            let param_type = self.resolve_type_from_path(param.ty)?;
            param_types.push(param_type);
        }

        // Handle return type from function definition
        let return_type = match &func_def.return_type.0 {
            Some((_, return_path)) => self.resolve_type_from_path(*return_path)?,
            None => Ty::mk_unit(self.tcx),
        };

        let func_type = Ty::mk_function(self.tcx, param_types, return_type);

        let func_name = func_name.raw;

        let func_identifier_id = self.tcx.alloc_identifier(func_name);
        env.bind(func_name, func_identifier_id, func_type);

        Ok(())
    }

    fn register_constant_signature(
        &mut self,
        env: &mut Environment<'cx, '_>,
        const_def: &ast::ConstantDef<'cx>,
    ) -> Result<()> {
        let const_name = const_def.name.raw;
        let declared_type = self.resolve_type_from_path(const_def.ty)?;

        let const_identifier_id = self.tcx.alloc_identifier(const_name);
        env.bind(const_name, const_identifier_id, declared_type);

        Ok(())
    }

    fn resolve_type_from_path(&self, path: ast::Path<'cx>) -> Result<Ty<'cx>> {
        match self.module_loader.resolve_path(path) {
            Some(item) => match item {
                opslang_ty::version::v1::ModuleItem::Type { ty, .. } => Ok(*ty),
                _ => Err(anyhow!("path `{path}` does not refer to a type")),
            },
            None => Err(anyhow!("unknown type: {path}")),
        }
    }

    fn resolve_path(&self, path: &'cx ast::Path<'cx>) -> Result<ResolvePathResult<'cx>> {
        match self.module_loader.resolve_path(*path) {
            Some(item) => {
                let resolved_path = ir::ResolvedPath {
                    item: opslang_ir::version::ResolvedItem::ModuleItem(item),
                    original_path: path,
                };
                Ok(ResolvePathResult {
                    resolved_path,
                    item,
                })
            }
            None => Err(anyhow!("cannot resolve path: {path}")),
        }
    }

    /// Eagerly resolves type variables in the given type using the provided substitution.
    fn eagerly_resolve(&self, subst: &mut Substitution<'cx>, ty: &mut Ty<'cx>) {
        // call `visit_ty_mut` to resolve type variables eagerly
        SubstitutionVisitor::new_borrowed(subst, self.tcx).visit_mut(ty);
    }

    fn try_external_resolve(&self, path: ast::Path<'cx>) -> Option<Ty<'cx>> {
        self.external_resolver.as_ref()?.resolve(path, self.tcx)
    }
}

/// Result of path resolution containing both the IR representation and the original item.
#[derive(Debug, Clone, Copy)]
struct ResolvePathResult<'cx> {
    /// The resolved path for IR generation
    resolved_path: ir::ResolvedPath<'cx>,
    /// The original module item with type information
    item: &'cx ModuleItem<'cx>,
}

#[cfg(test)]
mod tests {
    use opslang_ty::version::{IntTy, ModuleItem};

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

        if let Some(ModuleItem::Type { id, ty }) = builtin.lookup_item(parse_ident(&ast_cx, "i32"))
        {
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
        let builtin = create_builtin_module(&cx);

        let mut loader = ModuleLoader::new();
        loader.add_module(builtin);
        let mut _checker = TypeChecker::with_module_loader(loader, &cx, &ir_cx);

        let mut env = Environment::<'_, '_>::new();

        // ローカル変数 "i32" を定義（組み込み型をシャドーイング）
        let local_i32_type = Ty::mk_string(&cx);
        let local_id = cx.alloc_identifier("i32");
        env.bind("i32", local_id, local_i32_type);

        // パスを作成して型を解決
        // let path = ast::Path {
        //     raw: "i32",
        //     segments: &[],
        // };

        // let (resolved_type, _) = checker.typeck_variable(&cx, &env, &path).unwrap();

        // // ローカル変数が優先されるべき（String型になっている）
        // assert!(matches!(resolved_type.kind(), TyKind::String));
    }
}
