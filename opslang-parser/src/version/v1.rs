mod context;
mod generated;
mod parse;

pub use context::*;

crate::redirect_parol!();

use opslang_ast::di::Parse;

type This = super::V1;

pub struct ParserInput<'a> {
    pub string: &'a str,
    pub file_name: String,
}

impl<'cx> Parse<This> for opslang_ast::v1::Program<'cx> {
    type Format<'a>
        = ParserInput<'a>
    where
        Self: 'a;

    type Context = &'cx Context<'cx>;

    type Error = parol_runtime::ParolError;

    fn parse<'a>(
        ParserInput { string, file_name }: Self::Format<'a>,
        context: Self::Context,
    ) -> Result<Self, Self::Error>
    where
        Self: 'a,
    {
        let mut action = parse::Action::new(context);
        let string = context.alloc_str(string);
        let _ = generated::parser::parse(string, file_name, &mut action)?;
        todo!()
    }
}
