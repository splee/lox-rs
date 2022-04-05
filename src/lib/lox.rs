use crate::lib::ast::{Expr, ExprVisitor};

pub struct AstPrinter;
impl ExprVisitor<String> for AstPrinter {
    fn visit_expr(&mut self, e: &Expr) -> String {
        match e {
            Expr::Binary(left, operator, right) => format!(
                "({:?} {} {})",
                operator,
                self.visit_expr(left),
                self.visit_expr(right)
            ),
            Expr::Grouping(expr) => format!("(group {})", self.visit_expr(expr)),
            Expr::StringLiteral(value) => value.to_owned(),
            Expr::NumericLiteral(value) => format!("{}", value),
            Expr::Unary(operator, right) => format!("({:?} {})", operator, self.visit_expr(right)),
            Expr::BooleanLiteral(val) => format!("{}", val),
            Expr::NilLiteral => "nil".to_string(),
        }
    }
}
