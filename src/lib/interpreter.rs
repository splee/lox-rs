use crate::lib::{
    ast::{Expr, ExprVisitor, LiteralValue, Stmt, StmtVisitor},
    err::LoxError,
    object::Object,
    scanner::{Token, TokenType},
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
            Err(why) => Err(LoxError::Internal {
                message: format!("Failed to write to output: {:#?}", why),
            }),
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
            (Object::Number(l), Object::Number(r)) => match &op.token_type {
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
            },
            (Object::String(l), Object::String(r)) => match &op.token_type {
                TokenType::Plus => Object::String(format!("{}{}", l, r)),
                TokenType::EqualEqual => Object::Boolean(l.eq(r)),
                TokenType::BangEqual => Object::Boolean(!l.eq(r)),
                _ => return unsupported_operation_error("string", op),
            },
            (Object::Nil, Object::Nil) => {
                match &op.token_type {
                    TokenType::EqualEqual => Object::Boolean(true),
                    TokenType::BangEqual => Object::Boolean(false),
                    // (nil + nil) == (nil - nil) == (nil / nil) == (nil * nil) = true
                    TokenType::Plus | TokenType::Minus | TokenType::Slash | TokenType::Star => {
                        Object::Nil
                    }
                    _ => return unsupported_operation_error("nil", op),
                }
            }
            (Object::Boolean(l), Object::Boolean(r)) => match &op.token_type {
                TokenType::EqualEqual => Object::Boolean(l == r),
                TokenType::BangEqual => Object::Boolean(l != r),
                _ => return unsupported_operation_error("boolean", op),
            },
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
        let value = expression.accept(self)?;
        let stringified = format!("{}", value);
        self.write(&stringified)?;
        Ok(value)
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> Result<Object, LoxError> {
        if condition.accept(self)?.is_truthy() {
            Ok(then_branch.accept(self)?)
        } else {
            match else_branch {
                Some(branch_stmt) => Ok(branch_stmt.accept(self)?),
                None => Ok(Object::Nil),
            }
        }
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

    fn _parse_statements(src: &str) -> Result<Vec<Stmt>> {
        let tokens = scan(src).unwrap();
        match parse(&tokens) {
            Ok(v) => Ok(v),
            Err(why) => bail!(why),
        }
    }
    pub struct InterpreterTest {
        result: Result<Vec<Object>, LoxError>,
        out: Vec<u8>,
    }

    impl InterpreterTest {
        #[allow(dead_code)]
        fn from_statements(statements: &[Stmt]) -> Self {
            let mut out: Vec<u8> = Vec::new();
            let mut interpreter = Interpreter::new(&mut out);
            let result = interpreter.interpret(statements);

            InterpreterTest { result, out }
        }

        #[allow(dead_code)]
        fn from_src(src: &str) -> Result<Self> {
            let tokens = scan(src).unwrap();
            let statements = match parse(&tokens) {
                Ok(v) => v,
                Err(why) => bail!(why),
            };
            Ok(InterpreterTest::from_statements(&statements))
        }

        #[allow(dead_code)]
        fn assert_eq(
            self,
            expected_objects: &[Object],
            expected_output: Option<&str>,
        ) -> Result<()> {
            let objects = match self.result {
                Ok(v) => v,
                Err(why) => bail!(why),
            };

            assert_eq!(objects.len(), expected_objects.len());

            for (result, expected) in std::iter::zip(objects, expected_objects) {
                assert_eq!(&result, expected);
            }

            if let Some(expected_output) = expected_output {
                let output = String::from_utf8(self.out).unwrap();
                assert_eq!(output, expected_output);
            }
            Ok(())
        }

        #[allow(dead_code)]
        fn assert_err(self, message: &str) -> Result<()> {
            match self.result {
                Ok(v) => bail!("{} [Result Object: {:?}]", message, v),
                Err(_) => Ok(()),
            }
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

        let expected_objects = vec![
            Object::Number(150.6 * 2.0),
            Object::String("testing".to_owned()),
            Object::Number(3.0),
            Object::Boolean(false),
        ];

        let expected_output = Some("301.2\ntesting\n3\nfalse\n");

        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, expected_output)
    }

    #[test]
    fn test_small_script() -> Result<()> {
        let src = r#"
        150.6 * 2;
        "test" + "ing";
        1 + 2;
        1 == 0;
        "#;

        let expected_objects = vec![
            Object::Number(150.6 * 2.0),
            Object::String("testing".to_owned()),
            Object::Number(3.0),
            Object::Boolean(false),
        ];

        let expected_output = Some("");

        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, expected_output)
    }

    #[test]
    fn test_if_no_else_with_truthy_condition() -> Result<()> {
        let src = r#"
        if (true)
            1 + 1;
        "#;
        let expected_objects = vec![Object::Number(2.0)];
        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, None)
    }

    #[test]
    fn test_if_no_else_with_falsy_condition() -> Result<()> {
        let src = r#"
        if (false)
            1 + 1;
        "#;
        let expected_objects = vec![Object::Nil];
        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, None)
    }

    #[test]
    fn test_if_with_else_with_truthy_condition() -> Result<()> {
        let src = r#"
        if (true)
            1 + 1;
        else
           "test" + "ing";
        "#;
        let expected_objects = vec![Object::Number(2.0)];
        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, None)
    }

    #[test]
    fn test_if_with_else_with_falsy_condition() -> Result<()> {
        let src = r#"
        if (false)
            1 + 1;
        else
            "test" + "ing";
        "#;
        let expected_objects = vec![Object::String("testing".to_owned())];
        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, None)
    }
}
