use crate::lib::ast::{Expr, LiteralValue, Stmt};
use crate::lib::scanning::{Token, TokenType};
use crate::lib::err::LoxError;

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

    fn current(&mut self) -> Result<&Token, LoxError> {
        if self.is_consumed() {
            Err(LoxError::Internal {
                message: "current should always be valid but is out of bounds".to_owned(),
            })
        } else {
            Ok(&self.tokens[self.current_idx])
        }
    }

    fn previous(&mut self) -> Result<&Token, LoxError> {
        if self.current_idx > 0 {
            Ok(&self.tokens[self.current_idx - 1])
        } else {
            Err(LoxError::Internal {
                message: "previous should never be called without first calling advance".to_owned(),
            })
        }
    }

    fn peek(&mut self) -> Option<&Token> {
        if self.is_peekable() {
            Some(&self.tokens[self.current_idx])
        } else {
            None
        }
    }

    fn consume(&mut self, expected: &TokenType, message: &str) -> Result<(), LoxError> {
        if let Some(t) = self.peek() {
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
            let t = {
                if self.is_consumed() {
                    self.previous()?
                } else {
                    self.current()?
                }
            };
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

    fn is_peekable(&mut self) -> bool {
        !self.is_consumed()
    }

    fn is_consumed(&mut self) -> bool {
        self.current_idx >= self.tokens.len()
    }

    fn match_advance(&mut self, expected: TokenType) -> bool {
        if let Some(t) = self.peek() {
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
                message: "expected expr to be populated".to_string(),
            }),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, LoxError> {
    // TODO: Add error handling/parser synchronization
    let mut state = ParserState::new(tokens);
    while !state.is_consumed() {
        state = statement(state)?;
    }
    Ok(state.stmts)
}

fn statement(mut state: ParserState) -> Result<ParserState, LoxError> {
   let t = state.current()?;
   let is_print = matches!(&t.token_type, TokenType::Print);
   if is_print {
       // Skip the print token itself and parse the remainder of the tokens.
       state.advance();
   }
   let mut state = expression(state)?;
   let expr = state.take_expr()?;
   let stmt = match is_print {
       true => Stmt::Print(expr),
       false => Stmt::Expression(expr)
   };
   state.consume(&TokenType::Semicolon, "Expect ';' after statement")?;
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
        let operator = state.previous()?.clone();
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
        let operator = state.previous()?.clone();
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
        let operator = state.previous()?.clone();
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
        let operator = state.previous()?.clone();
        state = unary(state)?;
        let right = state.take_expr()?;
        expr = Box::new(Expr::Binary(expr, operator, right));
    }
    state.expr = Some(expr);
    Ok(state)
}

fn unary(mut state: ParserState) -> Result<ParserState, LoxError> {
    if state.match_advance2(TokenType::Bang, TokenType::Minus) {
        let operator = state.previous()?.clone();
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
    let t = state.current()?;
    let should_advance = !matches!(&t.token_type, TokenType::LeftParen);
    let raw_expr = match &t.token_type {
        TokenType::False => Expr::Literal(LiteralValue::Boolean(false)),
        TokenType::True => Expr::Literal(LiteralValue::Boolean(true)),
        TokenType::Nil => Expr::Literal(LiteralValue::Nil),
        TokenType::Str => Expr::Literal(LiteralValue::String(t.lexeme.clone())),
        TokenType::Numeric => {
            match t.lexeme.parse::<f64>() {
                Ok(num) => Expr::Literal(LiteralValue::Number(num)),
                Err(why) => {
                    return Err(LoxError::Parse {
                        line:t.line,
                        lexeme: t.lexeme.to_string(),
                        message: format!("Failed to parse numeric: {:#}", why)
                    });
                }
            }
        },
        TokenType::LeftParen => {
            state.advance();
            state = expression(state)?;
            state.consume(&TokenType::RightParen, "Expect ')' after expression")?;
            Expr::Grouping(state.take_expr()?)
        },
        _ => return Err(LoxError::Internal { message: format!("primary statement not found: {:?}", t)})
    };
    if should_advance {
        state.advance();
    }
    state.expr = Some(Box::new(raw_expr));
    Ok(state)
}

mod tests {
    use super::*;
    use anyhow::{bail, Result, Error};
    use crate::lib::{scanning::scan, lox::AstPrinter};

    /// A utility to build statements and compare to the expected
    /// statements.
    #[derive(Debug)]
    struct ParseTest {
        expected: Vec<Stmt>,
        result: Vec<Stmt>,
    }

    impl ParseTest {
        fn from_tokens(tokens: &[Token], expected: Vec<Stmt>) -> Result<Self> {
            let result = match parse(tokens) {
                Ok(r) => r,
                Err(why) => bail!(why),
            };
            Ok(Self { expected, result })
        }

        fn from_source(source: &str, expected: Vec<Stmt>) -> Result<Self> {
            let tokens = scan(source)?;
            match Self::from_tokens(&tokens, expected) {
                Ok(e) => Ok(e),
                Err(why) => bail!(why),
            }
        }

        fn assert_eq(self) -> Result<(), Error> {
            let mut printer = AstPrinter {};
            
            let result_strings = self.result
                .into_iter()
                .filter_map(|s| s.accept(&mut printer).ok())
                .collect::<Vec<String>>();
            
            let expected_strings = self.expected
                .into_iter()
                .filter_map(|s| s.accept(&mut printer).ok())
                .collect::<Vec<String>>();

            assert_eq!(expected_strings, result_strings);
            Ok(())
        }
    }

    #[test]
    fn test_parse_numeric_binary() -> Result<()> {
        let source = "1 + 1;";
        let expected = vec![
            Stmt::Expression(Box::new(
                Expr::Binary(
                    Box::new(Expr::Literal(LiteralValue::Number(1.0))),
                    Token { token_type: TokenType::Plus, lexeme: "+".to_owned(), line: 1, line_pos: 3 },
                    Box::new(Expr::Literal(LiteralValue::Number(1.0))),
                )
            ))
        ];
        ParseTest::from_source(source, expected)?
            .assert_eq()
    }

    #[test]
    fn test_parse_invalid_numbers() -> Result<()> {
        match ParseTest::from_source("1 1;", vec![]) {
            Err(_) => Ok(()),
            Ok(test) => {
                bail!("Invalid expression was parsed! {:#?}", test);
            }
        }
    }

    #[test]
    fn test_parse_semicolon_required() -> Result<()> {
        match ParseTest::from_source("1 + 1", vec![]) {
            Err(_) => Ok(()),
            Ok(r) => bail!("Statement should not parse: {:?}", r),
        }
    }
}