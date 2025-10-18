//! Example: Type check all .ops files in the example directory.
//!
//! Usage: cargo run --example v1_typeck_directory

use anyhow::Result;
use opslang_ast::syntax::v1 as ast;
use opslang_ir::version::v1 as ir;
use opslang_module::version::v1::ModuleContext;
use opslang_parser::{ParseOps, ParserInput};
use opslang_ty::version::v1::TypingContext;
use opslang_typeck::version::v1::{
    Session, TypeChecker, context::GlobalContext, create_builtin_module,
};
use std::fs;
use std::path::PathBuf;

fn main() -> Result<()> {
    // Use the directory where this example file is located
    let example_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("examples")
        .join("v1_typeck_directory")
        .join("example_root_dir");

    // Collect all .ops files in the example directory
    let mut ops_files = Vec::new();
    for entry in fs::read_dir(&example_dir)? {
        let entry = entry?;
        let path = entry.path();
        if path.is_file() && path.extension().is_some_and(|ext| ext == "ops") {
            ops_files.push(path);
        }
    }

    if ops_files.is_empty() {
        println!("No .ops files found in {}", example_dir.display());
        return Ok(());
    }

    println!("Found {} .ops file(s)", ops_files.len());

    // Set up contexts
    let ast_cx = ast::context::Context::new();
    let ir_cx = ir::context::Context::new();
    let tcx = TypingContext::new();
    let module_cx = ModuleContext::new();
    let gcx = GlobalContext {
        tcx: &tcx,
        module: &module_cx,
    };

    // Parse all files and create session
    let mut session = Session::new();
    for path in &ops_files {
        let source = fs::read_to_string(path)?;
        let input = ParserInput {
            content: ast_cx.alloc_str(&source),
            file_name: path.to_path_buf(),
        };
        let program = ast::Program::parse(input, &ast_cx)
            .map_err(|e| anyhow::anyhow!("Failed to parse '{}': {e}", path.display()))?;

        // Create module path from file name
        let file_name = path.file_stem().unwrap().to_str().unwrap();
        let module_path = module_cx.alloc_root_path(ast_cx.alloc_str(file_name));

        session.add_module(module_path, program);
        println!("Parsed: {}", path.display());
    }

    // Create type checker
    let mut typeck = TypeChecker::new(gcx, &ir_cx);
    typeck.add_module(create_builtin_module(gcx));

    // Type check all programs
    println!("\nType checking {} module(s)...", session.iter().count());
    typeck.typeck_programs(&mut session)?;

    println!("\nType checking completed successfully!");
    println!(
        "Generated {} IR program(s)",
        session
            .iter()
            .filter(|(_, entry)| entry.ir_program.is_some())
            .count()
    );

    Ok(())
}
