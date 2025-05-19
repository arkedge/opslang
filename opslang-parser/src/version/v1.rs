mod context;
mod generated;
mod parse;

pub use context::*;

crate::redirect_parol!();

use crate::ParseOps;

type This = super::V1;

pub struct ParserInput<'a> {
    pub string: &'a str,
    pub file_name: String,
}

impl<'a, 'cx> ParseOps<ParserInput<'a>, This> for opslang_ast::v1::Program<'cx> {
    type Context = &'cx ParseContext<'cx>;

    type Error = parol_runtime::ParolError;

    fn parse(
        ParserInput { string, file_name }: ParserInput<'a>,
        context: Self::Context,
    ) -> Result<Self, Self::Error> {
        let mut action = parse::Action::new(context);
        let string = context.alloc_str(string);
        let _ = generated::parser::parse(string, file_name, &mut action)?;
        todo!()
    }
}
