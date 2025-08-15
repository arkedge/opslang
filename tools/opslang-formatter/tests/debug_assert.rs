use opslang_ast::{
    DefinitionKind,
    v1::{ExprKind, Program, context::Context},
};
use opslang_formatter::{FormatterConfig, format_source};
use opslang_parser::{ParseOps, ParserInput};

#[test]
fn debug_assert_parentheses_behavior() {
    let config = FormatterConfig::default();

    let test_cases = [
        // Case 1: Simple assert with comparison
        "#! lang=v1\nprc main() { assert 2 == 2; }\n",
        // Case 2: Assert with explicit parentheses
        "#! lang=v1\nprc main() { assert (2 == 2); }\n",
        // Case 3: Simple assert with single value
        "#! lang=v1\nprc main() { assert true; }\n",
        // Case 4: Assert with complex expression
        "#! lang=v1\nprc main() { assert (1 + 1 == 2); }\n",
    ];

    for (i, input) in test_cases.iter().enumerate() {
        println!("\n=== Test Case {} ===", i + 1);
        println!("Input: {}", input.trim());

        // Format the input
        let output = format_source(input, &config).expect("Failed to format");
        println!("Output: {}", output.trim());

        // Debug AST structure
        let ctx = Context::new();
        let parser_input = ParserInput {
            content: input,
            file_name: "test.ops".into(),
        };
        let program: Program = Program::parse(parser_input, &ctx).expect("Failed to parse");

        if let Some(DefinitionKind::Function(f)) = &program.definitions[0].kind
            && let Some(first_item) = f.body.scope.items.get(1)
        {
            // Skip shebang
            if let opslang_ast::v1::ScopeItem::Row(row) = first_item
                && let Some(opslang_ast::v1::Statement::Expr(expr_stmt)) = &row.statement
            {
                println!("AST Type: {:?}", discriminant(&expr_stmt.expr.0));

                match &expr_stmt.expr.0 {
                    ExprKind::Apply(apply) => {
                        println!("Apply function: {:?}", apply.function);
                        println!("Apply args count: {}", apply.args.len());
                        if let Some(first_arg) = apply.args.first() {
                            println!("First arg type: {:?}", discriminant(&first_arg.0));
                            match &first_arg.0 {
                                ExprKind::Parened(parened) => {
                                    println!("  -> Argument is parenthesized");
                                    println!(
                                        "  -> Inner expr: {:?}",
                                        discriminant(&parened.expr.0)
                                    );
                                }
                                ExprKind::Compare(_) => {
                                    println!("  -> Argument is Compare (no parens)");
                                }
                                ExprKind::Binary(_) => {
                                    println!("  -> Argument is Binary (no parens)");
                                }
                                _ => {
                                    println!("  -> Argument is: {:?}", first_arg.0);
                                }
                            }
                        }
                    }
                    ExprKind::Compare(compare) => {
                        println!("Compare head: {:?}", discriminant(&compare.head.0));
                        println!("Compare tail count: {}", compare.tail_with_op.len());
                    }
                    _ => {
                        println!("Other expression type: {:?}", expr_stmt.expr.0);
                    }
                }
            }
        }

        println!("Expected behavior:");
        if input.contains("assert (") {
            println!("  -> Should preserve explicit parentheses");
        } else if input.contains("assert ") && input.contains(" == ") {
            println!("  -> Should NOT add extra parentheses");
        }
    }
}

fn discriminant<T>(val: &T) -> std::mem::Discriminant<T> {
    std::mem::discriminant(val)
}

#[test]
fn test_specific_assert_cases() {
    let config = FormatterConfig::default();

    // Test the exact case from test_v1.ops
    let input = "#! lang=v1\nprc main() {assert (2 == 2);}\n";
    let output = format_source(input, &config).expect("Failed to format");

    println!("Input: assert (2 == 2);");
    println!("Output: {}", output.trim());

    // This should preserve the parentheses as written
    // The issue is: why does it output "assert (2 == 2);" when input has explicit parens?
    // Answer: Because parser correctly sees Apply(assert, [Parened(Compare(...))])

    // Test without explicit parentheses
    let input2 = "#! lang=v1\nprc main() {assert 2 == 2;}\n";
    let output2 = format_source(input2, &config).expect("Failed to format");

    println!("Input2: assert 2 == 2;");
    println!("Output2: {}", output2.trim());

    // This should NOT add parentheses
    // The issue is: why does it output "assert (2 == 2);" for simple comparison?
    // Answer: Because parser sees Compare(Apply(assert, [2]), [(==, 2)])
    //         which means (assert 2) == 2, not assert(2 == 2)
}
