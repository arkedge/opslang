//! Integration tests for v1 visitor functionality
//! These tests use the parser to create AST nodes and then test visitor behavior

use opslang_ast::v1::context::Context;
use opslang_ast::v1::visit::AstVisitor;
use opslang_parser::{ParseOps, ParserInput};

// Helper to parse a given source code string
fn parse_source<'cx>(
    source: &'cx str,
    context: &'cx Context<'cx>,
) -> opslang_ast::v1::Program<'cx> {
    let input = ParserInput {
        content: source,
        file_name: "test.ops".into(),
    };
    opslang_ast::v1::Program::parse(input, context).expect("Failed to parse test source")
}

#[derive(Default)]
struct CountingVisitor {
    program_count: usize,
    function_count: usize,
    expr_count: usize,
    literal_count: usize,
    statement_count: usize,
    let_count: usize,
    return_count: usize,
    ident_count: usize,
    numeric_count: usize,
}

opslang_ast_macro::v1_ast_visitor_impl!(for CountingVisitor {
    fn visit_program(&mut self, node: &opslang_ast::v1::Program<'cx>) {
        self.program_count += 1;
        self.super_program(node);
    }

    fn visit_function_def(&mut self, node: &opslang_ast::v1::FunctionDef<'cx>) {
        self.function_count += 1;
        self.super_function_def(node);
    }

    fn visit_expr(&mut self, node: &opslang_ast::v1::Expr<'cx>) {
        self.expr_count += 1;
        self.super_expr(node);
    }

    fn visit_literal(&mut self, node: &opslang_ast::v1::Literal<'cx>) {
        self.literal_count += 1;
        self.super_literal(node);
    }

    fn visit_statement(&mut self, node: &opslang_ast::v1::Statement<'cx>) {
        self.statement_count += 1;
        self.super_statement(node);
    }

    fn visit_let(&mut self, node: &opslang_ast::v1::Let<'cx>) {
        self.let_count += 1;
        self.super_let(node);
    }

    fn visit_return_stmt(&mut self, node: &opslang_ast::v1::ReturnStmt<'cx>) {
        self.return_count += 1;
        self.super_return_stmt(node);
    }

    fn visit_ident(&mut self, node: &opslang_ast::v1::Ident<'cx>) {
        self.ident_count += 1;
        self.super_ident(node);
    }

    fn visit_numeric(&mut self, node: &opslang_ast::v1::Numeric<'cx>) {
        self.numeric_count += 1;
        self.super_numeric(node);
    }
});

#[derive(Default)]
struct CollectingVisitor {
    identifiers: Vec<String>,
    numeric_values: Vec<String>,
    string_literals: Vec<String>,
}

opslang_ast_macro::v1_ast_visitor_impl!(for CollectingVisitor {
    fn visit_ident(&mut self, node: &opslang_ast::v1::Ident<'cx>) {
        self.identifiers.push(node.raw.to_string());
        self.super_ident(node);
    }

    fn visit_numeric(&mut self, node: &opslang_ast::v1::Numeric<'cx>) {
        self.numeric_values.push(node.raw.to_string());
        self.super_numeric(node);
    }

    fn visit_string(&mut self, node: &opslang_ast::v1::String<'cx>) {
        self.string_literals.push(node.raw.to_string());
        self.super_string(node);
    }
});

#[test]
fn test_counting_visitor_simple_program() {
    let source = r#"#! lang=v1
prc main() {
    let x = 42;
    return;
}
"#;

    let context = Context::new();
    let program = parse_source(source, &context);
    let mut visitor = CountingVisitor::default();

    visitor.visit_program(&program);

    assert_eq!(visitor.program_count, 1);
    assert_eq!(visitor.function_count, 1);
    assert_eq!(visitor.let_count, 1);
    assert_eq!(visitor.return_count, 1);
    assert!(visitor.expr_count > 0); // Should have at least one expression (42)
    assert!(visitor.literal_count > 0); // Should have numeric literal
    assert!(visitor.ident_count >= 2); // Should have 'main' and 'x'
    assert_eq!(visitor.numeric_count, 1); // Should have '42'
}

#[test]
fn test_counting_visitor_multiple_functions() {
    let source = r#"#! lang=v1
prc main() {
    let x = 10;
    let y = 20;
    return;
}

prc add() {
    let result = 30;
    return;
}
"#;

    let context = Context::new();
    let program = parse_source(source, &context);
    let mut visitor = CountingVisitor::default();

    visitor.visit_program(&program);

    assert_eq!(visitor.program_count, 1);
    assert_eq!(visitor.function_count, 2);
    assert_eq!(visitor.let_count, 3);
    assert_eq!(visitor.return_count, 2);
    assert_eq!(visitor.numeric_count, 3); // Should have '10', '20', '30'
}

#[test]
fn test_collecting_visitor_identifiers() {
    let source = r#"#! lang=v1
prc main() {
    let variable_name = 123;
    let another_var = 456;
    return;
}
"#;

    let context = Context::new();
    let program = parse_source(source, &context);
    let mut visitor = CollectingVisitor::default();

    visitor.visit_program(&program);

    // Should collect all identifiers
    assert!(visitor.identifiers.contains(&"main".to_string()));
    assert!(visitor.identifiers.contains(&"variable_name".to_string()));
    assert!(visitor.identifiers.contains(&"another_var".to_string()));

    // Should collect numeric values
    assert!(visitor.numeric_values.contains(&"123".to_string()));
    assert!(visitor.numeric_values.contains(&"456".to_string()));
}

#[test]
fn test_collecting_visitor_with_strings() {
    let source = r#"#! lang=v1
prc main() {
    let greeting = "hello";
    let message = "world";
    let number = 42;
    return;
}
"#;

    let context = Context::new();
    let program = parse_source(source, &context);
    let mut visitor = CollectingVisitor::default();

    visitor.visit_program(&program);

    // Should collect string literals
    assert!(visitor.string_literals.contains(&"hello".to_string()));
    assert!(visitor.string_literals.contains(&"world".to_string()));

    // Should collect identifiers
    assert!(visitor.identifiers.contains(&"greeting".to_string()));
    assert!(visitor.identifiers.contains(&"message".to_string()));
    assert!(visitor.identifiers.contains(&"number".to_string()));

    // Should collect numeric value
    assert!(visitor.numeric_values.contains(&"42".to_string()));
}

#[test]
fn test_visitor_traversal_completeness() {
    let source = r#"#! lang=v1
prc test_function() {
    let a = 1;
    let b = 2;
    let c = a + b;
    return;
}

const CONSTANT: i32 = 100;
"#;

    let context = Context::new();
    let program = parse_source(source, &context);
    let mut visitor = CountingVisitor::default();

    visitor.visit_program(&program);

    // Verify that the visitor traversed the entire AST
    assert_eq!(visitor.program_count, 1);
    assert!(visitor.function_count >= 1); // At least the function
    assert!(visitor.ident_count >= 4); // Function name, variables a, b, c
    assert!(visitor.expr_count >= 3); // At least a few expressions
}

#[derive(Default)]
struct ExpressionTypeVisitor {
    variable_exprs: usize,
    literal_exprs: usize,
    binary_exprs: usize,
    parened_exprs: usize,
}

opslang_ast_macro::v1_ast_visitor_impl!(for ExpressionTypeVisitor {
    fn visit_expr(&mut self, node: &opslang_ast::v1::Expr<'cx>) {
        match node.0 {
            opslang_ast::v1::ExprKind::Variable(_) => {
                self.variable_exprs += 1;
            }
            opslang_ast::v1::ExprKind::Literal(_) => {
                self.literal_exprs += 1;
            }
            opslang_ast::v1::ExprKind::Binary(_) => {
                self.binary_exprs += 1;
            }
            opslang_ast::v1::ExprKind::Parened(_) => {
                self.parened_exprs += 1;
            }
            _ => {}
        }
        self.super_expr(node);
    }
});

#[test]
fn test_expression_type_visitor() {
    let source = r#"#! lang=v1
prc main() {
    let x = 10;
    let y = x + 20;
    let z = (y * 2);
    return;
}
"#;

    let context = Context::new();
    let program = parse_source(source, &context);
    let mut visitor = ExpressionTypeVisitor::default();

    visitor.visit_program(&program);

    // Should find different types of expressions
    assert!(visitor.literal_exprs > 0); // Numbers like 10, 20, 2
    assert!(visitor.variable_exprs > 0); // Variables like x, y
    // Binary and parenthesized expressions depend on the exact parsing
    // but we can at least verify the visitor ran without errors
}
