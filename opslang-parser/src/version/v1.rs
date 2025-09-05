mod generated;
mod parse;

crate::redirect_parol!();

use opslang_ast::v1::context::Context;

use crate::{ParseOps, ParserInput, Versioned};

type This = super::V1;

pub type AssumeV1Format<T> = Versioned<This, T>;

impl<'cx> ParseOps<AssumeV1Format<ParserInput<'cx>>, This> for opslang_ast::v1::Program<'cx> {
    type Context = &'cx Context<'cx>;

    type Error = parol_runtime::ParolError;

    fn parse(
        Versioned(ParserInput { content, file_name }, _): AssumeV1Format<ParserInput<'cx>>,
        context: Self::Context,
    ) -> Result<Self, Self::Error> {
        let mut action = parse::Action::new(context);
        let _ = generated::parser::parse(content, file_name, &mut action)?;
        Ok(action.finish())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_file() {
        let input_str = include_str!("../../tests/test_v1.ops");
        // input method 1
        let input = ParserInput {
            content: input_str,
            file_name: "test_v1.ops".into(),
        }
        .assume_inferred();
        let context = Context::new();
        let result = opslang_ast::v1::Program::parse(input, &context);
        assert!(
            result.is_ok(),
            "Failed to parse test_v1.ops: {:?}",
            result.err()
        );

        // input method 2
        let input = ParserInput {
            content: input_str,
            file_name: "test_v1.ops".into(),
        };
        let context = Context::new();
        let result = opslang_ast::v1::Program::parse(input, &context);
        assert!(
            result.is_ok(),
            "Failed to parse test_v1.ops: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_parse_integer_literals_with_suffixes() {
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

        let input = ParserInput {
            content: source,
            file_name: "test_integer_literals.ops".into(),
        };
        let context = Context::new();
        let result = opslang_ast::v1::Program::parse(input, &context);
        assert!(
            result.is_ok(),
            "Failed to parse integer literals: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_parse_float_literals_with_suffixes() {
        let source = r#"#! lang=v1
prc main() {
    let f32_val = 3.14f32;
    let f64_val = 2.71828f64;
    let float_var = 1.0f;
    return;
}
"#;

        let input = ParserInput {
            content: source,
            file_name: "test_float_literals.ops".into(),
        };
        let context = Context::new();
        let result = opslang_ast::v1::Program::parse(input, &context);
        assert!(
            result.is_ok(),
            "Failed to parse float literals: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_parse_duration_literals() {
        let source = r#"#! lang=v1
prc main() {
    let seconds = 30s;
    let milliseconds = 500ms;
    let microseconds = 1000us;
    let nanoseconds = 123456ns;
    return;
}
"#;

        let input = ParserInput {
            content: source,
            file_name: "test_duration_literals.ops".into(),
        };
        let context = Context::new();
        let result = opslang_ast::v1::Program::parse(input, &context);
        assert!(
            result.is_ok(),
            "Failed to parse duration literals: {:?}",
            result.err()
        );
    }

    #[test]
    fn test_parse_numeric_literals_without_suffixes() {
        let source = r#"#! lang=v1
prc main() {
    let int_val = 42;
    let float_val = 3.14;
    let sum = 10 + 20;
    let product = 2.5 * 4.0;
    return;
}
"#;

        let input = ParserInput {
            content: source,
            file_name: "test_numeric_literals.ops".into(),
        };
        let context = Context::new();
        let result = opslang_ast::v1::Program::parse(input, &context);
        assert!(
            result.is_ok(),
            "Failed to parse numeric literals without suffixes: {:?}",
            result.err()
        );
    }
}
