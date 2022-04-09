use crate::lib::{
    ast::{Expr, ExprVisitor, LiteralValue},
    err::LoxError,
    scanning::{Token, TokenType},
    object::Object,
};

pub struct Interpreter {}

impl Interpreter {
    pub fn evaluate(&mut self, expression: &Expr) -> Result<Object, LoxError> {
        expression.accept(self)
    }
}

impl ExprVisitor<Object> for Interpreter {

    fn visit_literal_expr(&mut self, value: &LiteralValue) -> Result<Object, LoxError> {
        match value {
            LiteralValue::Boolean(v) => Ok(Object::Boolean(v.clone())),
            LiteralValue::Nil => Ok(Object::Nil),
            LiteralValue::Number(v) => Ok(Object::Number(v.clone())),
            LiteralValue::String(v) => Ok(Object::String(v.clone())),
        }
    }

    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        op: &Token,
        right: &Expr,
    ) -> Result<Object, LoxError> {
        let left_eval = self.evaluate(left)?;
        let right_eval = self.evaluate(right)?;

        let object = match (&left_eval, &right_eval) {
            (Object::Number(l), Object::Number(r)) => {
                match &op.token_type {
                    TokenType::Plus => Object::Number(l + r),
                    TokenType::Minus => Object::Number(l - r),
                    TokenType::Slash => Object::Number(l / r),
                    TokenType::Star => Object::Number(l * r),
                    TokenType::Greater => Object::Boolean(l > r),
                    TokenType::GreaterEqual => Object::Boolean(l >= r),
                    TokenType::Less => Object::Boolean(l < r),
                    TokenType::LessEqual => Object::Boolean(l <= r),
                    TokenType::EqualEqual => Object::Boolean(l == r),
                    TokenType::BangEqual => Object::Boolean(l != r),
                    _ => return unsupported_operation_error("numeric", op),
                }
            },
            (Object::String(l), Object::String(r)) => {
                match &op.token_type {
                    TokenType::Plus => Object::String(format!("{}{}", l, r)),
                    TokenType::EqualEqual => Object::Boolean(l.eq(r)),
                    TokenType::BangEqual => Object::Boolean(!l.eq(r)),
                    _ => return unsupported_operation_error("string", op),
                }
            },
            (Object::Nil, Object::Nil) => {
                match &op.token_type {
                    TokenType::EqualEqual => Object::Boolean(true),
                    TokenType::BangEqual => Object::Boolean(false),
                    // (nil + nil) == (nil - nil) == (nil / nil) == (nil * nil) = true
                    TokenType::Plus | TokenType::Minus | TokenType::Slash | TokenType::Star => Object::Nil,
                    _ => return unsupported_operation_error("nil", op),
                }
            }
            // Not sure if gross or elegant...
            (_, _) => return unsupported_operation_error("supplied combination of", op),
        };
        Ok(object)
    }

    fn visit_grouping_expr(&mut self, left: &Expr) -> Result<Object, LoxError> {
        self.evaluate(left)
    }

    fn visit_unary_expr(&mut self, op: &Token, right: &Expr) -> Result<Object, LoxError> {
        let right_eval = self.evaluate(right)?;
        let return_expr = match right_eval {
            Object::Number(r) => {
                match &op.token_type {
                    TokenType::Minus => Object::Number(-r),
                    TokenType::Plus => Object::Number(r),
                    // A numeric is considered "truthy" if it is not equal to zero
                    TokenType::Bang => Object::Boolean(r != 0.0),
                    _ => return unsupported_operation_error("numeric", op),
                }
            }
            Object::Boolean(r) => match &op.token_type {
                TokenType::Bang => Object::Boolean(!r),
                _ => return unsupported_operation_error("boolean", op),
            },
            Object::Nil => match &op.token_type {
                TokenType::Bang => Object::Boolean(true),
                _ => return unsupported_operation_error("nil", op),
            },
            Object::String(_) => return unsupported_operation_error("string", op),
        };
        Ok(return_expr)
    }
}

fn unsupported_operation_error<T>(type_name: &str, op: &Token) -> Result<T, LoxError> {
    Err(LoxError::Runtime {
        message: format!("Unsupported operation for {} types.", type_name),
        at: op.clone(),
    })
}