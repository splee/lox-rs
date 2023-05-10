use crate::lib::{
    ast::{Expr, ExprVisitor, LiteralValue, Stmt, StmtVisitor},
    err::LoxError,
    object::Object,
    scanner::{Token, TokenType},
    environment::Environment,
};
use std::io::Write;
use std::collections::{HashMap, HashSet};

pub struct Interpreter<W: Write> {
    out: W,
    env: Environment,
}

impl<W: Write> Interpreter<W> {
    pub fn new(out: W) -> Self {
        Interpreter { out, env: Environment::new() }
    }

    pub fn evaluate(&mut self, expression: &Expr) -> Result<Object, LoxError> {
        expression.accept(self)
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Result<(), LoxError> {
        for stmt in statements {
            let _ = stmt.accept(self)?;
        }
        Ok(())
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

    fn visit_logical_expr(
        &mut self,
        _left: &Expr,
        _operator: &Token,
        _right: &Expr,
    ) -> Result<Object, LoxError> {
        todo!()
    }

    fn visit_variable_expr(&mut self, name: &Token) -> Result<Object, LoxError> {
        Ok(self.env.get(name)?.clone())
    }
}

impl<W: Write> StmtVisitor<()> for Interpreter<W> {
    fn visit_expression_stmt(&mut self, expression: &Expr) -> Result<(), LoxError> {
        let _ = self.evaluate(expression)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, expression: &Expr) -> Result<(), LoxError> {
        let value = expression.accept(self)?;
        let stringified = format!("{}", value);
        self.write(&stringified)?;
        Ok(())
    }

    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> Result<(), LoxError> {
        if condition.accept(self)?.is_truthy() {
            then_branch.accept(self)?;
            Ok(())
        } else {
            match else_branch {
                Some(branch_stmt) => {
                    branch_stmt.accept(self)?;
                    Ok(())
                },
                None => Ok(()),
            }
        }
    }

    fn visit_var_stmt(
        &mut self,
        name: &Token,
        initializer: Option<&Expr>,
    ) -> Result<(), LoxError> {
        let value = match initializer {
            Some(expr) => expr.accept(self)?,
            None => Object::Nil,
        };
        let _ = self.env.define(name, value)?;
        Ok(())
    }

    fn visit_assign_stmt(&mut self, name: &Token, expression: &Expr) -> Result<(), LoxError> {
        let value = expression.accept(self)?;
        let _ = self.env.assign(name, value)?;
        Ok(())
    }

    fn visit_block_stmt(&mut self, _statements: Vec<&Stmt>) -> Result<(), LoxError> {
        todo!()
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
        result: Result<(), LoxError>,
        out: Vec<u8>,
        env: Environment,
    }

    impl InterpreterTest {
        #[allow(dead_code)]
        fn from_statements(statements: &[Stmt]) -> Self {
            let mut out: Vec<u8> = Vec::new();
            let (result , env) = {
                let mut interpreter = Interpreter::new(&mut out);
                let r = interpreter.interpret(statements);
                let env = interpreter.env.clone();
                (r, env)
            };

            InterpreterTest { result, out, env }
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
            mut self,
            expected_objects: &HashMap<String, Object>,
            expected_output: Option<&str>,
        ) -> Result<()> {
            let _ = match self.result {
                Ok(v) => v,
                Err(why) => bail!(why),
            };

            let expected_vars: HashSet<String> = expected_objects.keys().cloned().collect();
            let result_vars: HashSet<String> = self.env.all_values().keys().cloned().collect();
            let diff: HashSet<_> = result_vars.symmetric_difference(&expected_vars).collect();
            assert_eq!(diff.len(), 0);

            for k in expected_objects.keys() {
                let expected_obj = expected_objects.get(k);
                let result_obj = self.env.get_by_name(k);
                assert_eq!(expected_obj, result_obj);
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

        let expected_objects = HashMap::new();
        let expected_output = Some("301.2\ntesting\n3\nfalse\n");

        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, expected_output)
    }

    #[test]
    fn test_small_script() -> Result<()> {
        let src = r#"
        var multiply = 150.6 * 2;
        var concat = "test" + "ing";
        var add = 1 + 2;
        var eq = 1 == 0;
        "#;

        let expected_objects = HashMap::from([
            ("multiply".to_string(), Object::Number(150.6 * 2.0)),
            ("concat".to_string(), Object::String("testing".to_owned())),
            ("add".to_string(), Object::Number(3.0)),
            ("eq".to_string(), Object::Boolean(false)),
        ]);

        let expected_output = Some("");

        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, expected_output)
    }

    #[test]
    fn test_declare_then_assign() -> Result<()> {
        let src = r#"
        var add;
        add = 1 + 1;
        "#;
        let expected_objects = HashMap::from([
            ("add".to_owned(), Object::Number(2.0)),
        ]);
        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, None)
    }

    #[test]
    fn test_if_no_else_with_truthy_condition() -> Result<()> {
        let src = r#"
        var add;
        if (true)
            add = 1 + 1;
        "#;
        let expected_objects = HashMap::from([
            ("add".to_string(), Object::Number(2.0)),
        ]);
        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, None)
    }

    #[test]
    fn test_if_no_else_with_falsy_condition() -> Result<()> {
        let src = r#"
        if (false)
            var add = 1 + 1;
        "#;
        let expected_objects = HashMap::new();
        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, None)
    }

    #[test]
    fn test_if_with_else_with_truthy_condition() -> Result<()> {
        let src = r#"
        if (true)
            var add = 1 + 1;
        else
            var concat = "test" + "ing";
        "#;
        let expected_objects = HashMap::from([
            ("add".to_string(), Object::Number(2.0)),
        ]);
        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, None)
    }

    #[test]
    fn test_if_with_else_with_falsy_condition() -> Result<()> {
        let src = r#"
        if (false)
            var add = 1 + 1;
        else
            var concat = "test" + "ing";
        "#;
        let expected_objects = HashMap::from([
            ("concat".to_string(), Object::String("testing".to_owned())),
        ]);
        InterpreterTest::from_src(src)?.assert_eq(&expected_objects, None)
    }
}
