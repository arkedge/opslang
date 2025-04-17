use super::generated::grammar_trait::{ActionTrait, Program};
#[allow(unused_imports)]
use parol_runtime::{Result, Token};
use std::fmt::{Debug, Display, Error, Formatter};

#[derive(Debug, Default)]
pub struct Action<'t> {
    pub my_grammar: Option<Program<'t>>,
}

impl Action<'_> {
    pub fn new() -> Self {
        Action::default()
    }
}

impl Display for Program<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        write!(f, "{:?}", self)
    }
}

impl Display for Action<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        match &self.my_grammar {
            Some(my_grammar) => writeln!(f, "{}", my_grammar),
            None => write!(f, "No parse result"),
        }
    }
}

impl<'t> ActionTrait<'t> for Action<'t> {
    fn program(&mut self, arg: &Program<'t>) -> Result<()> {
        self.my_grammar = Some(arg.clone());
        Ok(())
    }
}
