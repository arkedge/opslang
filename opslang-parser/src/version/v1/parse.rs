use super::{
    Context,
    generated::grammar_trait::{ActionTrait, Program},
};
#[allow(unused_imports)]
use parol_runtime::{Result, Token};

pub struct Action<'cx> {
    context: &'cx Context<'cx>,
}

impl<'cx> Action<'cx> {
    pub fn new(context: &'cx Context<'cx>) -> Self {
        Self { context }
    }
}

impl<'t> ActionTrait<'t> for Action<'t> {
    fn program(&mut self, arg: &Program<'t>) -> Result<()> {
        todo!()
    }
}
