use crate::lib::ast::{Expr, ExprVisitor, Op};
use crate::lib::scanning::TokenType;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Unsupported operation: {message} [{token_type}]")]
    UnsupportedOperation { message: String, token_type: String },
}

pub struct Interpreter {}

impl ExprVisitor<Result<Expr, RuntimeError>> for Interpreter {
    fn visit_expr(&mut self, expr: &Expr) -> Result<Expr, RuntimeError> {
        match expr {
            Expr::Binary(left, op, right) => self.visit_binary_expr(left, op, right),
            Expr::Grouping(left) => self.visit_grouping_expr(left),
            Expr::StringLiteral(v) => Ok(Expr::StringLiteral(v.clone())),
            Expr::NumericLiteral(v) => Ok(Expr::NumericLiteral(v.clone())),
            Expr::BooleanLiteral(v) => {
                match v {
                    true => Ok(Expr::BooleanLiteral(true)),
                    false => Ok(Expr::BooleanLiteral(false)),
                }
            },
            Expr::NilLiteral => Ok(Expr::NilLiteral),
            Expr::Unary(op, right) => self.visit_unary_expr(op, right),
        }
    }
}

impl Interpreter {
    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        op: &Op,
        right: &Expr,
    ) -> Result<Expr, RuntimeError> {
        let left_eval = self.visit_expr(left)?;
        let right_eval = self.visit_expr(right)?;

        let return_expr = match (&left_eval, &right_eval) {
            (Expr::NumericLiteral(l), Expr::NumericLiteral(r)) => {
                match &op.token_type {
                    TokenType::Plus => Expr::NumericLiteral(l + r),
                    TokenType::Minus => Expr::NumericLiteral(l - r),
                    TokenType::Slash => Expr::NumericLiteral(l / r),
                    TokenType::Star => Expr::NumericLiteral(l * r),
                    TokenType::Greater => Expr::BooleanLiteral(l > r),
                    TokenType::GreaterEqual => Expr::BooleanLiteral(l >= r),
                    TokenType::Less => Expr::BooleanLiteral(l < r),
                    TokenType::LessEqual => Expr::BooleanLiteral(l <= r),
                    TokenType::EqualEqual => Expr::BooleanLiteral(l == r),
                    TokenType::BangEqual => Expr::BooleanLiteral(l != r),
                    _ => return Err(RuntimeError::UnsupportedOperation{ message: "invalid for numeric types".to_owned(), token_type: format!("{:?}", &op.token_type) }),
                }
            },
            (Expr::StringLiteral(l), Expr::StringLiteral(r)) => {
                match &op.token_type {
                    TokenType::Plus => Expr::StringLiteral(format!("{}{}", l, r)),
                    TokenType::EqualEqual => Expr::BooleanLiteral(l.eq(r)),
                    TokenType::BangEqual => Expr::BooleanLiteral(!l.eq(r)),
                    _ => return Err(RuntimeError::UnsupportedOperation { message: "invalid for string types".to_owned(), token_type: format!("{:?}", &op.token_type) }),
                }
            },
            (Expr::NilLiteral, Expr::NilLiteral) => {
                match &op.token_type {
                    TokenType::EqualEqual => Expr::BooleanLiteral(true),
                    TokenType::BangEqual => Expr::BooleanLiteral(false),
                    // (nil + nil) == (nil - nil) == (nil / nil) == (nil * nil) = true
                    TokenType::Plus | TokenType::Minus | TokenType::Slash | TokenType::Star => Expr::NilLiteral,
                    _ => return Err(RuntimeError::UnsupportedOperation { message: "invalid for nil types".to_owned(), token_type: format!("{:?}", &op.token_type)}),
                }
            }
            (_, _) => return Err(RuntimeError::UnsupportedOperation { message: format!("Expression has no supported operations! Left operand: {:?}, Right operand: {:?}", left_eval, right_eval), token_type: format!("{:?}", &op.token_type) }),
        };
        Ok(return_expr)
    }

    fn visit_grouping_expr(&mut self, left: &Expr) -> Result<Expr, RuntimeError> {
        self.visit_expr(left)
    }

    fn visit_unary_expr(&mut self, op: &Op, right: &Expr) -> Result<Expr, RuntimeError> {
        let right_eval = self.visit_expr(right)?;
        let return_expr = match right_eval {
            Expr::NumericLiteral(r) => {
                match &op.token_type {
                    TokenType::Minus => Expr::NumericLiteral(-r),
                    TokenType::Plus => Expr::NumericLiteral(r),
                    // A numeric is considered "truthy" if it is not equal to zero
                    TokenType::Bang => Expr::BooleanLiteral(r != 0.0),
                    _ => {
                        return Err(RuntimeError::UnsupportedOperation {
                            message: "invalid for numeric types".to_owned(),
                            token_type: format!("{:?}", &op.token_type),
                        })
                    }
                }
            }
            Expr::BooleanLiteral(r) => match &op.token_type {
                TokenType::Bang => Expr::BooleanLiteral(!r),
                _ => {
                    return Err(RuntimeError::UnsupportedOperation {
                        message: "invalid for boolean types".to_owned(),
                        token_type: format!("{:?}", &op.token_type),
                    })
                }
            },
            Expr::NilLiteral => match &op.token_type {
                TokenType::Bang => Expr::BooleanLiteral(true),
                _ => {
                    return Err(RuntimeError::UnsupportedOperation {
                        message: "invalid for nil types".to_owned(),
                        token_type: format!("{:?}", &op.token_type),
                    })
                }
            },
            _ => {
                return Err(RuntimeError::UnsupportedOperation {
                    message: format!(
                        "Expression has no supported operations! Right operand: {:?}",
                        right_eval
                    ),
                    token_type: format!("{:?}", &op.token_type),
                })
            }
        };
        Ok(return_expr)
    }
}
