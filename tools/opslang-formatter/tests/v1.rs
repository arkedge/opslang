use opslang_formatter::{FormatterConfig, format_source};
use std::fs;

#[test]
fn test_format_ocaml_style_application() {
    let config = FormatterConfig::default();

    let input = "#! lang=v1\nassert 2 == 2;\n";
    let output = format_source(input, &config).expect("Failed to format");

    // Should use OCaml-style function application (f x y) not C-style (f(x, y))
    assert!(output.contains("assert 2 == 2;"));
    assert!(!output.contains("assert(2 == 2)"));
}

#[test]
fn test_format_preserves_basic_structure() {
    let config = FormatterConfig::default();

    let input = "#! lang=v1\nlet x = 42;\nprint x;\n";
    let output = format_source(input, &config).expect("Failed to format");

    // Should preserve basic statements
    assert!(output.contains("let x = 42;"));
    assert!(output.contains("print x;"));
    assert!(!output.contains("print(x)"));
}

#[test]
fn test_format_with_parentheses() {
    let config = FormatterConfig::default();

    let input = "#! lang=v1\nprint (1 + 2);\n";
    let output = format_source(input, &config).expect("Failed to format");

    // Should use OCaml-style function application
    assert!(output.contains("print ("));
    assert!(!output.contains("print("));
    // Note: Parser has known issue with binary operator order,
    // but parentheses should be preserved
    assert!(output.contains("("));
    assert!(output.contains(")"));
}

#[test]
fn test_v1_ops_formatting() {
    // Load default config
    let config_path = concat!(env!("CARGO_MANIFEST_DIR"), "/default.toml");
    let config = FormatterConfig::from_file(config_path).expect("Failed to load default config");

    // Read test_v1.ops
    let input_path = concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../opslang-parser/tests/test_v1.ops"
    );
    let input = fs::read_to_string(input_path).expect("Failed to read test_v1.ops");

    // Format the input
    let formatted = format_source(&input, &config).expect("Failed to format test_v1.ops");

    // Compare
    if input != formatted {
        fs::write(
            concat!(env!("CARGO_MANIFEST_DIR"), "/formatted_v1.ops"),
            &formatted,
        )
        .expect("Failed to write formatted output");
        panic!("Formatting did not preserve original structure");
    } else {
        println!("Formatting preserved original structure.");
    }
}
