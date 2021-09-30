use crate::ast::Program;

#[derive(Debug)]
pub enum TypecheckError {}

pub fn program(_program: &Program) -> Result<(), TypecheckError> {
    Ok(())
}
