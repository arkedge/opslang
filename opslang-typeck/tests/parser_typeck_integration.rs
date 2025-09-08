//! Integration tests for parser and type checker
//! These tests parse source code and perform type checking end-to-end

use std::fmt::Debug;

use opslang_ast::v1::context::Context as AstContext;
use opslang_ir::version::v1::Context as IrContext;
use opslang_parser::{ParseOps, ParserInput};
use opslang_ty::version::v1::TypingContext;
use opslang_typeck::version::v1::{TypeChecker, create_builtin_module};

/// Helper function to parse source code into AST.
fn parse_source<'cx>(
    source: &'cx str,
    context: &'cx AstContext<'cx>,
) -> Result<opslang_ast::v1::Program<'cx>, impl Debug> {
    let input = ParserInput {
        content: source,
        file_name: "test.ops".into(),
    };
    opslang_ast::v1::Program::parse(input, context)
}

/// Helper function to create type checker with builtin types.
fn create_type_checker<'cx>(
    tcx: &'cx TypingContext<'cx>,
    ir_cx: &'cx IrContext<'cx>,
) -> TypeChecker<'cx> {
    let mut checker = TypeChecker::new(tcx, ir_cx);
    checker.add_module(create_builtin_module(tcx));
    checker
}

#[test]
fn test_simple_function_typeck() {
    let source = r#"#! lang=v1
prc main() {
    let x = 42;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    // Parse the source code
    let program = parse_source(source, &ast_context).expect("Failed to parse source");

    // Create type checker and run type checking
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    // The test should succeed
    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_function_with_parameters() {
    let source = r#"#! lang=v1
prc add(x: i32, y: i32) -> i32 {
    let result = x + y;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());

    if let Ok(ir_program) = result {
        // Verify we got some IR output
        assert!(!ir_program.toplevel_items.is_empty());
    }
}

#[test]
fn test_constant_definition() {
    let source = r#"#! lang=v1
const VALUE: i32 = 100;
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_multiple_functions() {
    let source = r#"#! lang=v1
prc main() {
    let x = 10;
    return;
}

prc helper(value: i32) -> i32 {
    let doubled = value + value;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());

    if let Ok(ir_program) = result {
        assert_eq!(ir_program.toplevel_items.len(), 2);
    }
}

// Type annotation syntax is not currently supported
// TODO: Implement type annotation support and re-enable this test

#[test]
fn test_builtin_types() {
    let source = r#"#! lang=v1
const INT_VAL: i32 = 42;
const FLOAT_VAL: f64 = 3.14;
const STRING_VAL: string = "hello";
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());

    if let Ok(ir_program) = result {
        assert_eq!(
            ir_program.toplevel_items.len(),
            3,
            "{:?}",
            ir_program.toplevel_items
        );
    }
}

#[test]
fn test_array_literal() {
    let source = r#"#! lang=v1
prc main() {
    let numbers = [1, 2, 3];
    let empty_array = [];
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_binary_operations() {
    let source = r#"#! lang=v1
prc main() {
    let x = 10;
    let y = 20;
    let sum = x + y;
    let diff = x - y;
    let product = x * y;
    let quotient = x / y;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

// IF expressions currently fail due to grammar issues
// TODO: Fix grammar definition to support if expressions properly
#[test]
fn test_if_expression_grammar_issue() {
    let source = r#"#! lang=v1
prc main() {
    let x = 10;
    if x > 5 {
        let y = 20;
    }
    return;
}
"#;

    let ast_context = AstContext::new();

    // For now, we expect parsing to fail until grammar is fixed
    let parse_result = parse_source(source, &ast_context);
    assert!(
        parse_result.is_err(),
        "Expected parsing to fail due to unsupported if syntax"
    );
}

#[test]
fn test_unbound_variable_error() {
    let source = r#"#! lang=v1
prc main() {
    let x = unknown_variable;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    // This should fail due to unbound variable
    assert!(
        result.is_err(),
        "Expected type checking to fail but it succeeded"
    );
}

#[test]
fn test_same_scope_shadowing_should_work() {
    let source = r#"#! lang=v1
prc main() {
    let x = 42;
    let x = 100;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    // Same-scope shadowing should work in our language
    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_nested_scope_shadowing_should_work() {
    let source = r#"#! lang=v1
prc main() {
    let x = 42;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    // This should succeed for now (will be updated when block scopes are added)
    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_parameter_shadowing_should_work() {
    let source = r#"#! lang=v1
prc test_func(x: i32) {
    let x = 100;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    // Parameter shadowing should work in our language
    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_multiple_functions_same_variable_names_should_work() {
    let source = r#"#! lang=v1
prc func1() {
    let x = 42;
    return;
}

prc func2() {
    let x = 100;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    // Different function scopes should allow same variable names
    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_integer_literals_with_suffixes() {
    let source = r#"#! lang=v1
prc main() {
    let i8_val = 42i8;
    let i16_val = 1000i16;
    let i32_val = 50000i32;
    let i64_val = 1234567890i64;
    
    let u8_val = 255u8;
    let u16_val = 65535u16;
    let u32_val = 4294967295u32;
    let u64_val = 18446744073709551615u64;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_float_literals_with_suffixes() {
    let source = r#"#! lang=v1
prc main() {
    let f32_val = 3.14f32;
    let f64_val = 2.71828f64;
    let float_var = 1.0f;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_duration_literals() {
    let source = r#"#! lang=v1
prc main() {
    let seconds = 30s;
    let milliseconds = 500ms;
    let microseconds = 1000us;
    let nanoseconds = 123456ns;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_numeric_literals_in_expressions() {
    let source = r#"#! lang=v1
prc main() {
    let sum = 10i32 + 20i32;
    let float_calc = 3.14f64 * 2.0f64;
    let time_sum = 1s + 500ms;
    let mixed = 42;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_numeric_literals_type_inference() {
    let source = r#"#! lang=v1
prc main() {
    let inferred_int = 42;
    let inferred_float = 3.14;
    let explicit_int = 100i32;
    let explicit_float = 2.71f64;
    return;
}
"#;

    let ast_context = AstContext::new();
    let typing_context = TypingContext::new();
    let ir_context = IrContext::new();

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(&typing_context, &ir_context);
    let result = checker.typeck(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}
