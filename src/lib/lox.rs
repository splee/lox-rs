use crate::lib::{
    ast::{Expr, ExprVisitor, LiteralValue},
    scanning::Token,
    err::LoxError,
};

pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, expression: &Expr) -> Result<String, LoxError> {
        expression.accept(self)
    }
}

impl ExprVisitor<String> for AstPrinter {

    fn visit_binary_expr(&mut self, left: &Expr, operator: &Token, right: &Expr) -> Result<String, LoxError> {
        Ok(format!("({:?} {} {})", operator, left.accept(self)?, right.accept(self)?))
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
}
