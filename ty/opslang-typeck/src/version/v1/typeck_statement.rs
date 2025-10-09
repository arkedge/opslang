use super::*;

impl<'cx> TypeChecker<'cx> {
    pub(super) fn typeck_statement(
        &mut self,
        env: &mut Environment<'cx, '_>,
        subst: &mut Substitution<'cx>,
        stmt: &ast::Statement<'cx>,
    ) -> Result<ir::Statement<'cx>> {
        match stmt {
            ast::Statement::Let(let_stmt) => {
                let ir_rhs = self.typeck_expr(env, subst, &let_stmt.rhs)?;

                // Bind the variable to the environment with the inferred type
                let var_name = let_stmt.variable;
                let var_identifier_id = self.bind(env, var_name, ir_rhs.ty);

                Ok(ir::Statement::Let(ir::Let {
                    let_token: let_stmt.let_token.into_token(),
                    variable: var_identifier_id,
                    eq: let_stmt.eq.into_token(),
                    rhs: ir_rhs,
                    semi: let_stmt.semi.into_token(),
                }))
            }
            ast::Statement::Expr(expr_stmt) => {
                let ir_expr = self.typeck_expr(env, subst, &expr_stmt.expr)?;

                Ok(ir::Statement::Expr(ir::ExprStatement {
                    expr: ir_expr,
                    semi: expr_stmt.semi.into_token(),
                }))
            }
            ast::Statement::Return(ret_stmt) => Ok(ir::Statement::Return(ret_stmt.into_token())),
        }
    }
}
