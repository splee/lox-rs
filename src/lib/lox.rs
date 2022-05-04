use crate::lib::{
    ast::{Expr, ExprVisitor, LiteralValue, Stmt, StmtVisitor},
    err::LoxError,
    scanner::Token,
};

pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, statement: &Stmt) -> Result<String, LoxError> {
        statement.accept(self)
    }
}

impl ExprVisitor<String> for AstPrinter {
    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<String, LoxError> {
        Ok(format!(
            "({:?} {} {})",
            operator,
            left.accept(self)?,
            right.accept(self)?
        ))
    }

    fn visit_grouping_expr(&mut self, expression: &Expr) -> Result<String, LoxError> {
        Ok(format!("(group {})", expression.accept(self)?))
    }

    fn visit_literal_expr(&mut self, value: &LiteralValue) -> Result<String, LoxError> {
        let stringified = match value {
            LiteralValue::Boolean(v) => format!("{}", v),
            LiteralValue::Nil => String::from("nil"),
            LiteralValue::Number(v) => format!("{}", v),
            LiteralValue::String(v) => v.clone(),
        };
        Ok(stringified)
    }

    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> Result<String, LoxError> {
        Ok(format!("({:?} {})", operator, right.accept(self)?))
    }

    fn visit_logical_expr(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<String, LoxError> {
        todo!()
    }
}

impl StmtVisitor<String> for AstPrinter {
    fn visit_expression_stmt(&mut self, expression: &Expr) -> Result<String, LoxError> {
        Ok(format!("(stmt {})", expression.accept(self)?))
    }

    fn visit_print_stmt(&mut self, expression: &Expr) -> Result<String, LoxError> {
        Ok(format!("(print {})", expression.accept(self)?))
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> Result<String, LoxError> {
        let else_str = match else_branch {
            Some(v) => format!(" else {}", v.accept(self)?),
            None => "".to_owned(),
        };
        Ok(format!(
            "(if ({}) then {}{})",
            condition.accept(self)?,
            then_branch.accept(self)?,
            else_str
        ))
    }

    fn visit_var_stmt(&mut self, name: &Token, initializer: &Expr) -> Result<String, LoxError> {
        todo!()
    }
}
