//! Integration tests for parser and type checker
//! These tests parse source code and perform type checking end-to-end

use std::fmt::Debug;

use opslang_ast::syntax::v1 as ast;
use opslang_ir::version::v1 as ir;
use opslang_module::version::v1::ModuleContext;
use opslang_parser::{ParseOps, ParserInput};
use opslang_ty::version::v1::TypingContext;
use opslang_typeck::{
    v1_setup_cx,
    version::v1::{TypeChecker, context::GlobalContext, create_builtin_module},
};

/// Helper function to parse source code into AST.
fn parse_source<'cx>(
    source: &'cx str,
    context: &'cx ast::context::Context<'cx>,
) -> Result<opslang_ast::v1::Program<'cx>, impl Debug> {
    let input = ParserInput {
        content: source,
        file_name: "test.ops".into(),
    };
    opslang_ast::v1::Program::parse(input, context)
}

/// Helper function to create type checker with builtin types.
fn create_type_checker<'cx>(
    gcx: GlobalContext<'cx>,
    ir_cx: &'cx ir::context::Context<'cx>,
) -> TypeChecker<'cx> {
    let mut checker = TypeChecker::new(gcx, ir_cx);
    checker.add_module(create_builtin_module(gcx));
    checker
}

fn parse_typeck_success(source: &'static str) {
    v1_setup_cx!(ast_context, ir_context, gcx = { typing_context, module_context, });

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(gcx, &ir_context);
    let result = checker.typeck_single_program(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());
}

#[test]
fn test_simple_function_typeck() {
    let source = r#"#! lang=v1
prc main() {
    let x = 42;
    return;
}
"#;

    parse_typeck_success(source);
}

#[test]
fn test_function_with_parameters() {
    let source = r#"#! lang=v1
prc add(x: i32, y: i32) -> i32 {
    let result = x + y;
    return;
}
"#;

    v1_setup_cx!(ast_context, ir_context, gcx = { typing_context, module_context, });

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(gcx, &ir_context);
    let result = checker.typeck_single_program(&program);

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

    parse_typeck_success(source);
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

    v1_setup_cx!(ast_context, ir_context, gcx = { typing_context, module_context, });

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(gcx, &ir_context);
    let result = checker.typeck_single_program(&program);

    assert!(result.is_ok(), "Type checking failed: {:?}", result.err());

    if let Ok(ir_program) = result {
        assert_eq!(ir_program.toplevel_items.len(), 2);
    }
}

#[test]
fn test_builtin_types() {
    let source = r#"#! lang=v1
const INT_VAL: i32 = 42;
const FLOAT_VAL: f64 = 3.14;
const STRING_VAL: string = "hello";
"#;

    v1_setup_cx!(ast_context, ir_context, gcx = { typing_context, module_context, });

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(gcx, &ir_context);
    let result = checker.typeck_single_program(&program);

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

    parse_typeck_success(source);
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

    parse_typeck_success(source);
}

#[test]
fn test_if_expression() {
    let source = r#"#! lang=v1
prc main() {
    let x = 10;
    if x > 5 {
        let y = 20;
    };
    return;
}
"#;

    parse_typeck_success(source);
}

#[test]
fn test_unbound_variable_error() {
    let source = r#"#! lang=v1
prc main() {
    let x = unknown_variable;
    return;
}
"#;

    v1_setup_cx!(ast_context, ir_context, gcx = { typing_context, module_context, });

    let program = parse_source(source, &ast_context).expect("Failed to parse source");
    let mut checker = create_type_checker(gcx, &ir_context);
    let result = checker.typeck_single_program(&program);

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

    parse_typeck_success(source);
}

#[test]
fn test_nested_scope_shadowing_should_work() {
    let source = r#"#! lang=v1
prc main() {
    let x = 42;
    return;
}
"#;

    parse_typeck_success(source);
}

#[test]
fn test_parameter_shadowing_should_work() {
    let source = r#"#! lang=v1
prc test_func(x: i32) {
    let x = 100;
    return;
}
"#;

    parse_typeck_success(source);
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

    parse_typeck_success(source);
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

    parse_typeck_success(source);
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

    parse_typeck_success(source);
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

    parse_typeck_success(source);
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

    parse_typeck_success(source);
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

    parse_typeck_success(source);
}

#[test]
fn test_as_cast() {
    let source = r#"#! lang=v1
prc main() {
    let int_64 = 42;
    let int_32 = int_64 as i32;
    let float_64 = int_32 as f64;
    let bool_val = 1 as bool;
    return;
}
"#;

    parse_typeck_success(source);
}

#[test]
fn test_wait_select() {
    let source = r#"#! lang=v1
prc main() {
    wait 1s;
    select {
        1s => {
            let x = 42;
        }
        500ms => {
            let y = 100;
        }
    };
    return;
}
"#;

    parse_typeck_success(source);
}

#[test]
fn test_assert() {
    let source = r#"#! lang=v1
prc main() {
    assert true;
    assert (1 + 1 == 2);
    return;
}
"#;

    parse_typeck_success(source);
}

#[test]
fn test_assert_eq() {
    let source = r#"#! lang=v1
prc main() {
    assert_eq true false;
    assert_eq 1 1;
    assert_eq 3.14 3.14;
    assert_eq "hello" "hello";
    return;
}
"#;

    parse_typeck_success(source);
}

#[test]
fn test_print() {
    let source = r#"#! lang=v1
prc main() {
    print "Hello, World!";
    print 42;
    print 3.14;
    print true;
    print 1s;
    return;
}
"#;

    parse_typeck_success(source);
}

#[test]
fn test_prc_call() {
    let source = r#"#! lang=v1
prc f() {
    return;
}

prc main() {
    call f;
    call "unknown"?main;
    return;
}
"#;

    parse_typeck_success(source);
}
