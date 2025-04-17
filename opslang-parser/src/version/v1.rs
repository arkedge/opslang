mod context;
mod generated;
mod parse;

pub use context::*;

crate::redirect_parol!();

use opslang_ast::di::Parse;

type This = super::V1;

pub struct ParserInput<'cx> {
    pub string: &'cx str,
    pub file_name: String,
    pub context: &'cx Context<'cx>,
}

impl Parse<This> for opslang_ast::v1::Program<'_> {
    type Format<'a>
        = ParserInput<'a>
    where
        Self: 'a;

    type Error = parol_runtime::ParolError;

    fn parse<'a>(
        ParserInput {
            string,
            file_name,
            context,
        }: Self::Format<'a>,
    ) -> Result<Self, Self::Error>
    where
        Self: 'a,
    {
        let mut action = parse::Action::new();
        let _ = generated::parser::parse(string, file_name, &mut action)?;
        todo!()
    }
}
