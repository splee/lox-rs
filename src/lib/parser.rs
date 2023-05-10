use crate::lib::ast::{Expr, LiteralValue, Stmt};
use crate::lib::err::LoxError;
use crate::lib::scanner::{Token, TokenType};

struct ParserState<'a> {
    tokens: &'a [Token],
    current_idx: usize,
    expr: Option<Box<Expr>>,
    stmts: Vec<Stmt>,
}

impl<'a> ParserState<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        ParserState {
            tokens,
            current_idx: 0,
            expr: None,
            stmts: Vec::new(),
        }
    }

    fn try_current(&mut self) -> Result<&Token, LoxError> {
        match self.current() {
            Some(t) => Ok(t),
            None => Err(LoxError::Internal {
                message: "try_current should always be valid but is out of bounds".to_owned(),
            }),
        }
    }

    fn current(&mut self) -> Option<&Token> {
        if self.is_consumed() {
            None
        } else {
            Some(&self.tokens[self.current_idx])
        }
    }

    fn try_previous(&mut self) -> Result<&Token, LoxError> {
        match self.previous() {
            Some(t) => Ok(t),
            None => Err(LoxError::Internal {
                message: "try_previous should never be called without first calling advance"
                    .to_owned(),
            }),
        }
    }

    fn previous(&mut self) -> Option<&Token> {
        if self.current_idx > 0 {
            Some(&self.tokens[self.current_idx - 1])
        } else {
            None
        }
    }

    fn consume(&mut self, expected: &TokenType, message: &str) -> Result<(), LoxError> {
        if let Some(t) = self.current() {
            if expected.matches(&t.token_type) {
                self.advance();
                Ok(())
            } else {
                Err(LoxError::Parse {
                    line: t.line,
                    lexeme: t.lexeme.to_string(),
                    message: message.to_owned(),
                })
            }
        } else {
            let t = self.try_previous()?;
            Err(LoxError::Parse {
                line: t.line,
                lexeme: t.lexeme.to_string(),
                message: message.to_owned(),
            })
        }
    }

    fn advance(&mut self) -> bool {
        if self.is_consumed() {
            false
        } else {
            self.current_idx += 1;
            true
        }
    }

    fn is_consumed(&mut self) -> bool {
        self.current_idx >= self.tokens.len()
    }

    fn match_advance(&mut self, expected: TokenType) -> bool {
        if let Some(t) = self.current() {
            if expected.matches(&t.token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn match_advance2(&mut self, t1: TokenType, t2: TokenType) -> bool {
        self.match_advance(t1) || self.match_advance(t2)
    }

    fn match_advance4(
        &mut self,
        t1: TokenType,
        t2: TokenType,
        t3: TokenType,
        t4: TokenType,
    ) -> bool {
        self.match_advance2(t1, t2) || self.match_advance2(t3, t4)
    }

    fn take_expr(&mut self) -> Result<Box<Expr>, LoxError> {
        match self.expr.take() {
            Some(v) => Ok(v),
            None => Err(LoxError::Internal {
                message: "expected expr to be populated".to_owned(),
            }),
        }
    }

    fn take_stmt(&mut self) -> Result<Box<Stmt>, LoxError> {
        match self.stmts.pop() {
            Some(v) => Ok(Box::new(v)),
            None => Err(LoxError::Internal {
                message: "expected stmt to be populated".to_owned(),
            }),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, LoxError> {
    // TODO: Add error handling/parser synchronization
    let mut state = ParserState::new(tokens);
    while !state.is_consumed() {
        state = declaration(state)?;
    }
    Ok(state.stmts)
}

fn declaration(mut state: ParserState) -> Result<ParserState, LoxError> {
    let t = state.try_current()?;
    state = match &t.token_type {
        &TokenType::Var => var_declaration(state)?,
        &TokenType::Identifier => var_assignment(state)?,
        _ => statement(state)?,
    };
    // TODO: syncronize() here
    // error recovery: https://www.craftinginterpreters.com/parsing-expressions.html
    // parsing variables: https://www.craftinginterpreters.com/statements-and-state.html#parsing-variables
    Ok(state)
}

fn var_declaration(mut state: ParserState) -> Result<ParserState, LoxError> {
    // Skip the Var token
    state.advance();
    state.consume(&TokenType::Identifier, "Expect variable name.")?;
    let name_token = state.try_previous()?.clone();

    let t = state.try_current()?;
    let initializer = match &t.token_type {
        &TokenType::Equal => {
            state.advance();
            state = expression(state)?;
            let expr = state.take_expr()?;
            Some(expr)
        }
        _ => None,
    };
    state.consume(
        &TokenType::Semicolon,
        "Expect ';' after variable declaration.",
    )?;
    let stmt = Stmt::Var(name_token, initializer);
    state.stmts.push(stmt);

    Ok(state)
}

fn var_assignment(mut state: ParserState) -> Result<ParserState, LoxError> {
    state.consume(&TokenType::Identifier, "Expect variable name.")?;
    // Get the variable name.
    let name_token = state.try_previous()?.clone();

    // Consume the = between identifier and expression.
    state.consume(&TokenType::Equal, "Expect equal for assignment to variable.")?;

    // Get the right hand side expression
    state = expression(state)?;
    let expr = state.take_expr()?;

    // Be sure we've got a semicolon next.
    state.consume(&TokenType::Semicolon, "Expect ';' after variable assignment.")?;

    let stmt = Stmt::Assign(name_token, expr);
    state.stmts.push(stmt);

    Ok(state)
}

fn statement(mut state: ParserState) -> Result<ParserState, LoxError> {
    let t = state.try_current()?;
    state = match &t.token_type {
        TokenType::If => if_statement(state)?,
        TokenType::Print => print_statement(state)?,
        TokenType::LeftBrace => block_statement(state)?,
        // TODO: This feels like a bug waiting to happen...
        _ => expression_statement(state)?,
    };
    Ok(state)
}

fn expression_statement(mut state: ParserState) -> Result<ParserState, LoxError> {
    state = expression(state)?;
    let expr = state.take_expr()?;
    let stmt = Stmt::Expression(expr);
    state.consume(&TokenType::Semicolon, "Expect ';' after statement")?;
    state.stmts.push(stmt);
    Ok(state)
}

fn print_statement(mut state: ParserState) -> Result<ParserState, LoxError> {
    // Consume the print token
    state.advance();
    state = expression(state)?;
    let expr = state.take_expr()?;
    let stmt = Stmt::Print(expr);
    state.consume(&TokenType::Semicolon, "Expect ';' after statement")?;
    state.stmts.push(stmt);
    Ok(state)
}

fn block_statement(mut state: ParserState) -> Result<ParserState, LoxError> {
    // Consume the left brace.
    state.advance();
    let mut statements: Vec<Box<Stmt>> = Vec::new();
    loop {
        if let Some(t) = state.current() {
            if let TokenType::RightBrace = t.token_type {
                break;
            } else {
                state = declaration(state)?;
                statements.push(state.take_stmt()?);
            }
        } else {
            break;
        }
    } 
    state.consume(&TokenType::RightBrace, "Expect '}' after block")?;
    let stmt = Stmt::Block(statements);
    state.stmts.push(stmt);
    Ok(state)
}

fn if_statement(mut state: ParserState) -> Result<ParserState, LoxError> {
    // Consume the if token
    state.advance();
    // Next should be an open paren
    state.consume(&TokenType::LeftParen, "Expect '(' after 'if'.")?;
    state = expression(state)?;
    state.consume(&TokenType::RightParen, "Expect ')' after if condition.")?;
    let condition = state.take_expr()?;

    state = declaration(state)?;
    let then_branch = state.take_stmt()?;

    let else_branch = {
        if state.match_advance(TokenType::Else) {
            state = declaration(state)?;
            Some(state.take_stmt()?)
        } else {
            None
        }
    };

    let stmt = Stmt::If(condition, then_branch, else_branch);
    state.stmts.push(stmt);
    Ok(state)
}

fn expression(state: ParserState) -> Result<ParserState, LoxError> {
    equality(state)
}

fn equality(mut state: ParserState) -> Result<ParserState, LoxError> {
    state = comparison(state)?;
    let mut expr = state.take_expr()?;
    while state.match_advance2(TokenType::BangEqual, TokenType::EqualEqual) {
        let operator = state.try_previous()?.clone();
        state = comparison(state)?;
        let right = state.take_expr()?;
        expr = Box::new(Expr::Binary(expr, operator, right));
    }
    state.expr = Some(expr);
    Ok(state)
}

fn comparison(mut state: ParserState) -> Result<ParserState, LoxError> {
    state = term(state)?;
    let mut expr = state.take_expr()?;

    while state.match_advance4(
        TokenType::Greater,
        TokenType::GreaterEqual,
        TokenType::Less,
        TokenType::LessEqual,
    ) {
        let operator = state.try_previous()?.clone();
        state = term(state)?;
        let right = state.take_expr()?;
        expr = Box::new(Expr::Binary(expr, operator, right));
    }
    state.expr = Some(expr);
    Ok(state)
}

fn term(mut state: ParserState) -> Result<ParserState, LoxError> {
    state = factor(state)?;
    let mut expr = state.take_expr()?;

    while state.match_advance2(TokenType::Minus, TokenType::Plus) {
        let operator = state.try_previous()?.clone();
        state = factor(state)?;
        let right = state.take_expr()?;
        expr = Box::new(Expr::Binary(expr, operator, right));
    }
    state.expr = Some(expr);
    Ok(state)
}

fn factor(mut state: ParserState) -> Result<ParserState, LoxError> {
    state = unary(state)?;
    let mut expr = state.take_expr()?;

    while state.match_advance2(TokenType::Slash, TokenType::Star) {
        let operator = state.try_previous()?.clone();
        state = unary(state)?;
        let right = state.take_expr()?;
        expr = Box::new(Expr::Binary(expr, operator, right));
    }
    state.expr = Some(expr);
    Ok(state)
}

fn unary(mut state: ParserState) -> Result<ParserState, LoxError> {
    if state.match_advance2(TokenType::Bang, TokenType::Minus) {
        let operator = state.try_previous()?.clone();
        state = unary(state)?;
        let right = state.take_expr()?;
        let expr = Box::new(Expr::Unary(operator, right));
        state.expr = Some(expr);
        Ok(state)
    } else {
        primary(state)
    }
}

fn primary(mut state: ParserState) -> Result<ParserState, LoxError> {
    let t = state.try_current()?;
    let should_advance = !matches!(&t.token_type, TokenType::LeftParen);
    let raw_expr = match &t.token_type {
        TokenType::False => Expr::Literal(LiteralValue::Boolean(false)),
        TokenType::True => Expr::Literal(LiteralValue::Boolean(true)),
        TokenType::Nil => Expr::Literal(LiteralValue::Nil),
        TokenType::Str => Expr::Literal(LiteralValue::String(t.lexeme.clone())),
        TokenType::Numeric => match t.lexeme.parse::<f64>() {
            Ok(num) => Expr::Literal(LiteralValue::Number(num)),
            Err(why) => {
                return Err(LoxError::Parse {
                    line: t.line,
                    lexeme: t.lexeme.to_string(),
                    message: format!("Failed to parse numeric: {:#}", why),
                });
            }
        },
        TokenType::LeftParen => {
            state.advance();
            state = expression(state)?;
            state.consume(&TokenType::RightParen, "Expect ')' after expression")?;
            Expr::Grouping(state.take_expr()?)
        }
        _ => {
            return Err(LoxError::Internal {
                message: format!("primary statement not found: {:?}", t),
            })
        }
    };
    if should_advance {
        state.advance();
    }
    state.expr = Some(Box::new(raw_expr));
    Ok(state)
}

mod tests {
    use super::*;
    use crate::lib::{lox::AstPrinter, scanner::scan};
    use anyhow::{bail, Result};

    /// A utility to build statements and compare to the expected
    /// statements.
    #[derive(Debug)]
    #[allow(dead_code)]
    struct ParseTest {
        result: Result<Vec<Stmt>, LoxError>,
    }

    impl ParseTest {
        #[allow(dead_code)]
        fn from_tokens(tokens: &[Token]) -> Self {
            let result = parse(tokens);
            Self { result }
        }

        #[allow(dead_code)]
        fn from_source(source: &str) -> Self {
            let tokens = scan(source).unwrap();
            Self::from_tokens(&tokens)
        }

        #[allow(dead_code)]
        fn assert_eq(self, expected: &[Stmt]) -> Result<()> {
            let mut printer = AstPrinter {};
            let result = match self.result {
                Ok(v) => v,
                Err(why) => bail!(why),
            };

            let result_strings = result
                .into_iter()
                .filter_map(|s| s.accept(&mut printer).ok())
                .collect::<Vec<String>>();

            let expected_strings = expected
                .into_iter()
                .filter_map(|s| s.accept(&mut printer).ok())
                .collect::<Vec<String>>();

            assert_eq!(expected_strings, result_strings);
            Ok(())
        }

        #[allow(dead_code)]
        fn assert_err(self, message: &str) -> Result<()> {
            match self.result {
                Ok(v) => bail!("{} [Result value: {:?}]", message, v),
                Err(_) => Ok(()),
            }
        }
    }

    #[test]
    fn test_parse_numeric_binary() -> Result<()> {
        let source = "1 + 1;";
        let expected = vec![Stmt::Expression(Box::new(Expr::Binary(
            Box::new(Expr::Literal(LiteralValue::Number(1.0))),
            Token {
                token_type: TokenType::Plus,
                lexeme: "+".to_owned(),
                line: 1,
                line_pos: 3,
            },
            Box::new(Expr::Literal(LiteralValue::Number(1.0))),
        )))];
        ParseTest::from_source(source).assert_eq(&expected)
    }

    #[test]
    fn test_parse_invalid_numbers() -> Result<()> {
        ParseTest::from_source("1 1;").assert_err("Invalid expression was parsed!")
    }

    #[test]
    fn test_parse_semicolon_required() -> Result<()> {
        ParseTest::from_source("1 + 1").assert_err("Statement should not parse!")
    }

    #[test]
    fn test_if_with_no_else_branch() -> Result<()> {
        let source = "if (true) 1 + 1;";
        let condition = Expr::Literal(LiteralValue::Boolean(true));
        let then_branch = Stmt::Expression(Box::new(Expr::Binary(
            Box::new(Expr::Literal(LiteralValue::Number(1.0))),
            Token {
                token_type: TokenType::Plus,
                lexeme: "+".to_owned(),
                line: 1,
                line_pos: 13,
            },
            Box::new(Expr::Literal(LiteralValue::Number(1.0))),
        )));
        let expected = vec![Stmt::If(Box::new(condition), Box::new(then_branch), None)];
        ParseTest::from_source(source).assert_eq(&expected)
    }

    #[test]
    fn test_if_with_else_branch() -> Result<()> {
        let source = "if (true) 1 + 1; else nil;";
        let condition = Expr::Literal(LiteralValue::Boolean(true));
        let then_branch = Stmt::Expression(Box::new(Expr::Binary(
            Box::new(Expr::Literal(LiteralValue::Number(1.0))),
            Token {
                token_type: TokenType::Plus,
                lexeme: "+".to_owned(),
                line: 1,
                line_pos: 13,
            },
            Box::new(Expr::Literal(LiteralValue::Number(1.0))),
        )));
        let else_branch = Stmt::Expression(Box::new(Expr::Literal(LiteralValue::Nil)));
        let expected = vec![Stmt::If(
            Box::new(condition),
            Box::new(then_branch),
            Some(Box::new(else_branch)),
        )];
        ParseTest::from_source(source).assert_eq(&expected)
    }

    #[test]
    fn test_if_with_missing_closing_paren_on_condition() -> Result<()> {
        let source = "if (true 1 + 1;";
        ParseTest::from_source(source).assert_err("Invalid if statement parsed!")
    }

    #[test]
    fn test_block_definition() -> Result<()> {
        let source = "{ var test; }";
        let expected = vec![Stmt::Block(
            vec![
                Box::new(Stmt::Var(Token {
                    token_type: TokenType::Var,
                    lexeme: "test".to_owned(),
                    line: 1,
                    line_pos: 13,
                }, None))
            ]
        )];
        ParseTest::from_source(source).assert_eq(&expected)
    }

    #[test]
    fn test_block_without_enclosing_brace() -> Result<()> {
        let source = "{ var test;";
        ParseTest::from_source(source).assert_err("Block not closed, parsing should fail!")
    }

    #[test]
    fn test_multi_var_statement() -> Result<()> {
        let source = r#"
        var multiply = 150.6 * 2;
        var concat = "test" + "ing";
        var add = 1 + 2;
        var eq = 1 == 0;
        "#;
        let multiply_var = Stmt::Var(
            Token { token_type: TokenType::Var, lexeme: "multiply".to_owned(), line: 1, line_pos: 5},
            Some(Box::new(
                Expr::Binary(
                    Box::new(Expr::Literal(LiteralValue::Number(150.6))),
                    Token {
                        token_type: TokenType::Star,
                        lexeme: "*".to_owned(),
                        line: 2,
                        line_pos: 30,
                    },
                    Box::new(Expr::Literal(LiteralValue::Number(2.0))),
                )
            ))
        );
        let concat_var = Stmt::Var(
            Token { token_type: TokenType::Var, lexeme: "concat".to_owned(), line: 1, line_pos: 5},
            Some(Box::new(
                Expr::Binary(
                    Box::new(Expr::Literal(LiteralValue::String("test".to_owned()))),
                    Token {
                        token_type: TokenType::Plus,
                        lexeme: "+".to_owned(),
                        line: 3,
                        line_pos: 29,
                    },
                    Box::new(Expr::Literal(LiteralValue::String("ing".to_owned()))),
                ),
            ))
        );
        let add_var = Stmt::Var(
            Token { token_type: TokenType::Var, lexeme: "add".to_owned(), line: 1, line_pos: 1},
            Some(Box::new(
                Expr::Binary(
                    Box::new(Expr::Literal(LiteralValue::Number(1.0))),
                    Token {
                        token_type: TokenType::Plus,
                        lexeme: "+".to_owned(),
                        line: 4,
                        line_pos: 21,
                    },
                    Box::new(Expr::Literal(LiteralValue::Number(2.0))),
                ),
            ))
        );
        let eq_var = Stmt::Var(
            Token { token_type: TokenType::Var, lexeme: "eq".to_owned(), line: 1, line_pos: 1},
            Some(Box::new(
                Expr::Binary(
                    Box::new(Expr::Literal(LiteralValue::Number(1.0))),
                    Token {
                        token_type: TokenType::EqualEqual,
                        lexeme: "==".to_owned(),
                        line: 5,
                        line_pos: 20,
                    },
                    Box::new(Expr::Literal(LiteralValue::Number(0.0))),
                ),
            ))
        );
        let expected = vec![multiply_var, concat_var, add_var, eq_var];
        ParseTest::from_source(source).assert_eq(&expected)
    }
}
