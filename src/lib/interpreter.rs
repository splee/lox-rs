use crate::lib::{
    ast::{Expr, ExprVisitor, LiteralValue, Stmt, StmtVisitor},
    err::LoxError,
    scanner::{Token, TokenType},
    object::Object,
};
use std::io::Write;

pub struct Interpreter<W: Write> {
    out: W,
}

impl<W: Write> Interpreter<W> {

    pub fn new(out: W) -> Self {
        Interpreter { out }
    }

    pub fn evaluate(&mut self, expression: &Expr) -> Result<Object, LoxError> {
        expression.accept(self)
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<Vec<Object>, LoxError> {
        let mut objects: Vec<Object> = Vec::new();
        for stmt in statements {
            objects.push(stmt.accept(self)?);
        }
        Ok(objects)
    }

    fn write(&mut self, value: &str) -> Result<(), LoxError> {
        match write!(self.out, "{}\n", value) {
            Ok(_) => Ok(()),
            Err(why) => Err(LoxError::Internal { message: format!("Failed to write to output: {:#?}", why) }),
        }
    }
}

impl<W: Write> ExprVisitor<Object> for Interpreter<W> {

    fn visit_literal_expr(&mut self, value: &LiteralValue) -> Result<Object, LoxError> {
        match value {
            LiteralValue::Boolean(v) => Ok(Object::Boolean(*v)),
            LiteralValue::Nil => Ok(Object::Nil),
            LiteralValue::Number(v) => Ok(Object::Number(*v)),
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
                    TokenType::EqualEqual => Object::Boolean((l - r).abs() < f64::EPSILON),
                    TokenType::BangEqual => Object::Boolean((l - r).abs() > f64::EPSILON),
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
            (Object::Boolean(l), Object::Boolean(r)) => {
                match &op.token_type {
                    TokenType::EqualEqual => Object::Boolean(l == r),
                    TokenType::BangEqual => Object::Boolean(l != r),
                    _ => return unsupported_operation_error("boolean", op),
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

impl<W: Write> StmtVisitor<Object> for Interpreter<W> {
    fn visit_expression_stmt(&mut self, expression: &Expr) -> Result<Object, LoxError> {
        let value = self.evaluate(expression)?;
        Ok(value)
    }

    fn visit_print_stmt(&mut self, expression: &Expr) -> Result<Object, LoxError> {
        let value = self.visit_expression_stmt(expression)?;
        let stringified = format!("{}", value);
        self.write(&stringified)?;
        Ok(value)
    }
}

fn unsupported_operation_error<T>(type_name: &str, op: &Token) -> Result<T, LoxError> {
    Err(LoxError::Runtime {
        message: format!("Unsupported operation for {} types.", type_name),
        at: op.clone(),
    })
}

mod tests {
    use crate::lib::{parser::parse, scanner::scan};

    use super::*;
    use anyhow::{bail, Result};
    use std::io::stdout;

    fn _interpret(statements: Vec<Stmt>) -> Result<Vec<Object>> {
        let mut interpreter = Interpreter::new(stdout());

        let objects = match interpreter.interpret(&statements) {
            Ok(v) => v,
            Err(why) => bail!(why),
        };
        Ok(objects)
    }

    fn _parse_statements(src: &str) -> Result<Vec<Stmt>> {
        let tokens = scan(src)?;
        match parse(&tokens) {
            Ok(v) => Ok(v),
            Err(why) => bail!(why),
        }
    }

    #[test]
    fn test_small_print_script() -> Result<()> {
        let src = r#"
            print 150.6 * 2;
            print "test" + "ing";
            print 1 + 2;
            print 1 == 0;
        "#;
        let statements = _parse_statements(src)?;

        let mut mock_writer: Vec<u8> = Vec::new();
        let mut interp = Interpreter::new(&mut mock_writer);
        let objects = match interp.interpret(&statements) {
            Ok(v) => v,
            Err(why) => bail!(why),
        };

        let expected_objects = vec![
            Object::Number(150.6 * 2.0),
            Object::String("testing".to_owned()),
            Object::Number(3.0),
            Object::Boolean(false),
        ];

        assert_eq!(objects.len(), expected_objects.len());

        for (result, expected) in std::iter::zip(objects, expected_objects) {
            assert_eq!(result, expected);
        }

        let expected_output = "301.2\ntesting\n3\nfalse\n";

        assert_eq!(String::from_utf8(mock_writer).unwrap(), expected_output);
        Ok(())
    }

    #[test]
    fn test_small_script() -> Result<()> {
        let src = r#"
        150.6 * 2;
        "test" + "ing";
        1 + 2;
        1 == 0;
        "#;
        let statements = _parse_statements(src)?;

        let mut mock_writer: Vec<u8> = Vec::new();
        let mut interp = Interpreter::new(&mut mock_writer);
        let objects = match interp.interpret(&statements) {
            Ok(v) => v,
            Err(why) => bail!(why),
        };

        let expected_objects = vec![
            Object::Number(150.6 * 2.0),
            Object::String("testing".to_owned()),
            Object::Number(3.0),
            Object::Boolean(false),
        ];

        assert_eq!(objects.len(), expected_objects.len());

        for (result, expected) in std::iter::zip(objects, expected_objects) {
            assert_eq!(result, expected);
        }

        let expected_output = "";

        assert_eq!(String::from_utf8(mock_writer).unwrap(), expected_output);

        Ok(())
    }
}