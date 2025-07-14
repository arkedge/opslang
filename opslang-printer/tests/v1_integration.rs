//! Integration tests for v1 printer functionality
//! These tests use the parser to create AST nodes and then test printer behavior

use opslang_ast::v1::context::Context;
use opslang_parser::{ParseOps, ParserInput};
use opslang_printer::{
    BasePrintOptions, CommentAligned, CommentAlignment, CommentGrouping, CommentPosition, Naive,
    PrettyPrint, PrintOptions,
};

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

#[test]
fn test_naive_vs_comment_aligned_simple() {
    let source = r#"#! lang=v1
let x = value1;               # comment1
let very_long_var = value2;   # comment2
"#;

    let context = Context::new();
    let program = parse_source(source, &context);

    // Test with Naive strategy
    let naive_options = PrintOptions::<Naive>::default();
    let naive_result = program.to_pretty_string(&naive_options);

    // Test with CommentAligned strategy
    let aligned_options = PrintOptions::<CommentAligned>::default();
    let aligned_result = program.to_pretty_string(&aligned_options);

    println!("Naive result:\n{naive_result}");
    println!("Aligned result:\n{aligned_result}");

    // Both should contain the same content but with different comment alignment
    assert!(naive_result.contains("let x = value1"));
    assert!(aligned_result.contains("let x = value1"));
    assert!(naive_result.contains("comment1"));
    assert!(aligned_result.contains("comment1"));

    // The aligned version should have different spacing
    assert_ne!(naive_result, aligned_result);
}

#[test]
fn test_consecutive_vs_per_block_grouping() {
    let source = r#"#! lang=v1
let x = val1;           # comment1
let very_long_var = val2;   # comment2

let y = val3;               # comment3
"#;

    let context = Context::new();
    let program = parse_source(source, &context);

    // Test with Consecutive grouping
    let consecutive_options = PrintOptions::<CommentAligned>::from_base_with_alignment(
        BasePrintOptions::default(),
        CommentAlignment {
            grouping: CommentGrouping::Consecutive,
            position: CommentPosition::ToLongest,
        },
    );
    let consecutive_result = program.to_pretty_string(&consecutive_options);

    // Test with PerBlock grouping
    let per_block_options = PrintOptions::<CommentAligned>::from_base_with_alignment(
        BasePrintOptions::default(),
        CommentAlignment {
            grouping: CommentGrouping::PerBlock,
            position: CommentPosition::ToLongest,
        },
    );
    let per_block_result = program.to_pretty_string(&per_block_options);

    println!("Consecutive result:\n{consecutive_result}");
    println!("Per block result:\n{per_block_result}");

    // Both should contain the same content
    assert!(consecutive_result.contains("let x = val1"));
    assert!(per_block_result.contains("let x = val1"));

    // The grouping should be different
    assert_ne!(consecutive_result, per_block_result);
}

#[test]
fn test_position_strategies() {
    let source = r#"#! lang=v1
let x = val1;               # comment1
let very_long_variable = val2;  # comment2
"#;

    let context = Context::new();
    let program = parse_source(source, &context);

    // Test ToLongest
    let longest_options = PrintOptions::<CommentAligned>::from_base_with_alignment(
        BasePrintOptions::default(),
        CommentAlignment {
            grouping: CommentGrouping::Consecutive,
            position: CommentPosition::ToLongest,
        },
    );
    let longest_result = program.to_pretty_string(&longest_options);

    // Test ToFixed
    let fixed_options = PrintOptions::<CommentAligned>::from_base_with_alignment(
        BasePrintOptions::default(),
        CommentAlignment {
            grouping: CommentGrouping::Consecutive,
            position: CommentPosition::ToFixed {
                column: 30,
                fallback_to_longest: false,
            },
        },
    );
    let fixed_result = program.to_pretty_string(&fixed_options);

    // Test ToTabMultiple
    let tab_options = PrintOptions::<CommentAligned>::from_base_with_alignment(
        BasePrintOptions::default(),
        CommentAlignment {
            grouping: CommentGrouping::Consecutive,
            position: CommentPosition::ToTabMultiple {
                tab_size: 8,
                fallback_to_longest: false,
            },
        },
    );
    let tab_result = program.to_pretty_string(&tab_options);

    println!("ToLongest result:\n{longest_result}");
    println!("ToFixed result:\n{fixed_result}");
    println!("ToTabMultiple result:\n{tab_result}");

    // All should contain the same content
    assert!(longest_result.contains("let x = val1"));
    assert!(fixed_result.contains("let x = val1"));
    assert!(tab_result.contains("let x = val1"));

    // The alignment should be different
    assert_ne!(longest_result, fixed_result);
    assert_ne!(longest_result, tab_result);
    assert_ne!(fixed_result, tab_result);
}

#[test]
fn test_fallback_to_longest_behavior() {
    let source = r#"#! lang=v1
let x = val1;                               # comment1
let this_is_a_very_very_long_variable_name = val2;  # comment2
"#;

    let context = Context::new();
    let program = parse_source(source, &context);

    // Test ToFixed with fallback disabled
    let fixed_no_fallback = PrintOptions::<CommentAligned>::from_base_with_alignment(
        BasePrintOptions::default(),
        CommentAlignment {
            grouping: CommentGrouping::Consecutive,
            position: CommentPosition::ToFixed {
                column: 20,
                fallback_to_longest: false,
            },
        },
    );
    let fixed_no_fallback_result = program.to_pretty_string(&fixed_no_fallback);

    // Test ToFixed with fallback enabled
    let fixed_with_fallback = PrintOptions::<CommentAligned>::from_base_with_alignment(
        BasePrintOptions::default(),
        CommentAlignment {
            grouping: CommentGrouping::Consecutive,
            position: CommentPosition::ToFixed {
                column: 20,
                fallback_to_longest: true,
            },
        },
    );
    let fixed_with_fallback_result = program.to_pretty_string(&fixed_with_fallback);

    println!("Fixed no fallback result:\n{fixed_no_fallback_result}");
    println!("Fixed with fallback result:\n{fixed_with_fallback_result}");

    // Both should contain the same content
    assert!(fixed_no_fallback_result.contains("let x = val1"));
    assert!(fixed_with_fallback_result.contains("let x = val1"));

    // The fallback behavior should be different
    assert_ne!(fixed_no_fallback_result, fixed_with_fallback_result);
}

#[test]
fn test_empty_lines_with_consecutive_grouping() {
    let source = r#"#! lang=v1
let x = val1;       # comment1
let y = val2;       # comment2

let very_long_var = val3;  # comment3
let z = val4;       # comment4
"#;

    let context = Context::new();
    let program = parse_source(source, &context);

    let consecutive_options = PrintOptions::<CommentAligned>::from_base_with_alignment(
        BasePrintOptions::default(),
        CommentAlignment {
            grouping: CommentGrouping::Consecutive,
            position: CommentPosition::ToLongest,
        },
    );
    let result = program.to_pretty_string(&consecutive_options);

    println!("Empty lines with consecutive grouping result:\n{result}");

    // Should contain all content
    assert!(result.contains("let x = val1"));
    assert!(result.contains("let y = val2"));
    assert!(result.contains("let very_long_var = val3"));
    assert!(result.contains("let z = val4"));

    // Should contain all comments
    assert!(result.contains("comment1"));
    assert!(result.contains("comment2"));
    assert!(result.contains("comment3"));
    assert!(result.contains("comment4"));

    // Should have empty line separating groups
    assert!(result.contains("\n\n"));
}
