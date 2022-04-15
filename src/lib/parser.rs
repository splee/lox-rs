use crate::lib::ast::{Expr, LiteralValue, Stmt};
use crate::lib::scanning::{Token, TokenType};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("{line:?} at '{lexeme:?}': {msg:?}")]
    Panic {
        line: usize,
        lexeme: String,
        msg: String,
    },

    #[error("An internal parser error occurred: {msg:?}")]
    InternalError { msg: String },
}

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

    fn current(&mut self) -> Result<&Token, ParseError> {
        if self.is_consumed() {
            Err(ParseError::InternalError {
                msg: "current should always be valid but is out of bounds".to_owned(),
            })
        } else {
            Ok(&self.tokens[self.current_idx])
        }
    }

    fn previous(&mut self) -> Result<&Token, ParseError> {
        if self.current_idx > 0 {
            Ok(&self.tokens[self.current_idx - 1])
        } else {
            Err(ParseError::InternalError {
                msg: "previous should never be called without first calling advance".to_owned(),
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

    fn consume(&mut self, expected: &TokenType, message: &str) -> Result<(), ParseError> {
        if let Some(t) = self.peek() {
            if expected.matches(&t.token_type) {
                self.advance();
                Ok(())
            } else {
                Err(ParseError::Panic {
                    line: t.line,
                    lexeme: t.lexeme.to_string(),
                    msg: message.to_owned(),
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
            Err(ParseError::Panic {
                line: t.line,
                lexeme: t.lexeme.to_string(),
                msg: message.to_owned(),
            })
        }
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

    fn take_expr(&mut self) -> Result<Box<Expr>, ParseError> {
        match self.expr.take() {
            Some(v) => Ok(v),
            None => Err(ParseError::InternalError {
                msg: "expected expr to be populated".to_string(),
            }),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, ParseError> {
    // TODO: Add error handling/parser synchronization
    let mut state = ParserState::new(tokens);
    while !state.is_consumed() {
        state = statement(state)?;
    }
    Ok(state.stmts)
}

fn statement(mut state: ParserState) -> Result<ParserState, ParseError> {
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

fn expression(state: ParserState) -> Result<ParserState, ParseError> {
    equality(state)
}

fn equality(mut state: ParserState) -> Result<ParserState, ParseError> {
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

fn comparison(mut state: ParserState) -> Result<ParserState, ParseError> {
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

fn term(mut state: ParserState) -> Result<ParserState, ParseError> {
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

fn factor(mut state: ParserState) -> Result<ParserState, ParseError> {
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

fn unary(mut state: ParserState) -> Result<ParserState, ParseError> {
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

fn primary(mut state: ParserState) -> Result<ParserState, ParseError> {
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
                    return Err(ParseError::Panic {
                        line:t.line,
                        lexeme: t.lexeme.to_string(),
                        msg: format!("Failed to parse numeric: {:#}", why)
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
        _ => return Err(ParseError::InternalError { msg: format!("primary statement not found: {:?}", t)})
    };
    if should_advance {
        state.advance();
    }
    state.expr = Some(Box::new(raw_expr));
    Ok(state)
}
