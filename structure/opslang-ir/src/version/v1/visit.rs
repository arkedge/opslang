#[opslang_ir_macro::v1_declare_ir_visitor_trait]
pub trait IrVisitor<'cx> {}

#[opslang_ir_macro::v1_declare_ir_visitor_mut_trait]
pub trait IrMutVisitor<'cx> {}
