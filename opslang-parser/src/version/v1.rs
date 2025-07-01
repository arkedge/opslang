mod context;
mod generated;
mod parse;

pub use context::*;

crate::redirect_parol!();

use crate::{ParseOps, ParserInput, Versioned};

type This = super::V1;

pub type AssumeV1Format<T> = Versioned<This, T>;

impl<'cx> ParseOps<AssumeV1Format<ParserInput<'cx>>, This> for opslang_ast::v1::Program<'cx> {
    type Context = &'cx ParseContext<'cx>;

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
        let context = ParseContext::new();
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
        let context = ParseContext::new();
        let result = opslang_ast::v1::Program::parse(input, &context);
        assert!(
            result.is_ok(),
            "Failed to parse test_v1.ops: {:?}",
            result.err()
        );
    }
}
