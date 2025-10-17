use anyhow::anyhow;
use ast::token::IntoToken;
use chrono::Utc;
use opslang_ast::v1::{self as ast};
use opslang_ir::version::v1::{self as ir};

use ir::Typed;
use opslang_module::version::v1::{Module, ModuleDef, ModuleItem, ModuleItemDef, ModuleLoader};
use opslang_ty::version::v1::{
    self as ty, FloatTy, Ident, IntTy, PolyTy, Procedure, Substitution, Ty, TyKind, TyVid,
    TypingContext,
};
use opslang_visitor::VisitorMut;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};

type Result<T, E = anyhow::Error> = std::result::Result<T, E>;

mod environment;
use environment::Environment;

mod hm;
pub use hm::generalize_ty;

mod lower;
mod register_signature;
mod resolve;
mod typeck_apply;
mod typeck_binary;
mod typeck_block;
mod typeck_call;
mod typeck_constant;
mod typeck_expr;
mod typeck_function;
mod typeck_literal;
mod typeck_path;
mod typeck_statement;

mod builtin_module;
pub use builtin_module::create_builtin_module;

pub mod context;
use context::GlobalContext;

/// The main type checker that performs type inference and checking.
pub struct TypeChecker<'cx> {
    /// Module loader for resolving external symbols.
    module_loader: ModuleLoader<'cx>,

    /// Optional external resolver for custom type resolution.
    external_resolver: Option<Box<dyn ExternalResolver<'cx>>>,

    gcx: GlobalContext<'cx>,
    ir_cx: &'cx ir::Context<'cx>,
}

impl<'cx> Deref for TypeChecker<'cx> {
    type Target = GlobalContext<'cx>;

    fn deref(&self) -> &Self::Target {
        &self.gcx
    }
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
    fn resolve(&self, path: ast::Path<'cx>, tcx: &'cx TypingContext<'cx>) -> Option<Ty<'cx>>;
}

impl<'cx> TypeChecker<'cx> {
    /// Creates a new type checker with empty module loader.
    ///
    /// The type checker starts with no modules loaded.
    pub fn new(gcx: GlobalContext<'cx>, ir_cx: &'cx ir::Context<'cx>) -> Self {
        Self {
            external_resolver: None,
            module_loader: ModuleLoader::new(),
            gcx,
            ir_cx,
        }
    }

    /// Creates a new type checker with the given module loader.
    ///
    /// This allows pre-loading modules before starting type checking operations.
    pub fn with_module_loader(
        module_loader: ModuleLoader<'cx>,
        gcx: GlobalContext<'cx>,
        ir_cx: &'cx ir::Context<'cx>,
    ) -> Self {
        Self {
            external_resolver: None,
            module_loader,
            gcx,
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
    pub fn add_module(&mut self, module: Module<'cx>) {
        self.module_loader.add_module(module);
    }

    /// Performs type checking on multiple AST programs and converts them to IR.
    ///
    /// This is the entry point for type checking multiple files.
    /// It first collects all signatures from all programs, then performs type checking
    /// on each program's items with the unified global environment.
    pub fn typeck_programs(
        &mut self,
        programs: &[ast::Program<'cx>],
    ) -> Result<Vec<ir::Program<'cx>>> {
        // First pass: collect all signatures from all programs
        let mut global_env = Environment::new();
        for program in programs {
            for definition in program.toplevel_items {
                self.register_signature(&mut global_env, definition)?;
            }
        }

        // Second pass: type check each program with the unified global environment
        let mut ir_programs = Vec::new();
        for program in programs {
            let ir_program = self.typeck_items_with_global_env(&global_env, program)?;
            ir_programs.push(ir_program);
        }

        Ok(ir_programs)
    }

    /// Performs type checking on an AST program and converts it to IR.
    ///
    /// This is the main entry point for type checking and IR generation.
    /// It processes all top-level definitions, performs type inference,
    /// and converts the AST to a typed IR representation.
    pub fn typeck_single_program(
        &mut self,
        program: &ast::Program<'cx>,
    ) -> Result<ir::Program<'cx>> {
        // First pass: collect all function and constant signatures
        let global_env = self.collect_signatures(program)?;

        // Second pass: type check implementations and lower comments
        self.typeck_items_with_global_env(&global_env, program)
    }

    // Note: 'env can be any lifetime, even that of later borrowing from the *returned value*.
    fn collect_signatures<'env>(
        &mut self,
        program: &ast::Program<'cx>,
    ) -> Result<Environment<'cx, 'env>> {
        // Create global environment for top-level definitions
        let mut global_env = Environment::new();

        for definition in program.toplevel_items {
            self.register_signature(&mut global_env, definition)?;
        }
        Ok(global_env)
    }

    fn typeck_items_with_global_env<'env>(
        &mut self,
        global_env: &'env Environment<'cx, 'env>,
        program: &ast::Program<'cx>,
    ) -> Result<ir::Program<'cx>> {
        let mut ir_definitions = Vec::new();
        let mut pending_comments: Vec<&'cx ast::Comment<'cx>> = Vec::new();

        for toplevel_item in program.toplevel_items {
            if toplevel_item.is_empty() {
                // Empty item (empty line) - discard any pending comments
                pending_comments.clear();
                continue;
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
                        let ir_func = self.typeck_function(global_env, func_def)?;
                        ir::DefinitionKind::Function(ir_func)
                    }
                    ast::DefinitionKind::Constant(const_def) => {
                        let ir_const = self.typeck_constant(global_env, const_def)?;
                        ir::DefinitionKind::Constant(ir_const)
                    }
                };

                let ir_definition = ir::Definition {
                    comment_before: leading_comment,
                    comment_trailing: toplevel_item
                        .comment
                        .map(|ast_comment| lower::lower_comment(self.ir_cx, ast_comment)),
                    kind: ir_kind,
                };

                ir_definitions.push(ir_definition);
                pending_comments.clear();
            } else if let Some(comment) = &toplevel_item.comment {
                // Collect comment from this item if present
                pending_comments.push(comment);
            }
        }

        Ok(ir::Program {
            toplevel_items: self.ir_cx.alloc_definition_slice(ir_definitions),
        })
    }
}

/// Result of path resolution containing both the IR representation and the original item.
#[derive(Debug, Clone, Copy)]
struct ResolvePathResult<'cx> {
    /// The resolved path for IR generation
    resolved_path: ir::ResolvedPath<'cx>,
    /// The original module item with type information
    item: ModuleItem<'cx>,
}

#[cfg(test)]
mod tests;

#[macro_export]
/// Shorthand to set up a type checking contexts.
///
/// Internal use only but exposed for use in tests.
macro_rules! v1_setup_cx {
    ($ast_context:ident, $ir_context:ident, $gcx:ident = { $typing_context:ident, $module_context:ident $(,)? }) => {
        let $ast_context = ast::context::Context::new();
        let $ir_context = ir::context::Context::new();
        let $typing_context = TypingContext::new();
        let $module_context = ModuleContext::new();
        let $gcx = GlobalContext {
            tcx: &$typing_context,
            module: &$module_context,
        };
    };
}
