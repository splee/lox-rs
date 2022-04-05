use crate::lib::ast::{Expr, Op};
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
}

impl<'a> ParserState<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        ParserState {
            tokens,
            current_idx: 0,
            expr: None,
        }
    }

    fn current(&self) -> Result<&Token, ParseError> {
        if self.is_consumed() {
            return Err(ParseError::InternalError {
                msg: "current should always be valid but is out of bounds".to_owned(),
            });
        }
        Ok(&self.tokens[self.current_idx])
    }

    fn previous(&self) -> Result<&Token, ParseError> {
        if self.current_idx > 0 {
            Ok(&self.tokens[self.current_idx - 1])
        } else {
            Err(ParseError::InternalError {
                msg: "previous should never be called without first calling advance".to_owned(),
            })
        }
    }

    fn peek(&self) -> Option<&Token> {
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
                return Ok(());
            } else {
                return Err(ParseError::Panic {
                    line: t.line,
                    lexeme: t.lexeme.to_string(),
                    msg: message.to_owned(),
                });
            }
        } else {
            let t = self.current()?;
            return Err(ParseError::Panic {
                line: t.line,
                lexeme: t.lexeme.to_string(),
                msg: message.to_owned(),
            });
        }
    }

    fn match_advance(&mut self, expected: TokenType) -> bool {
        if let Some(t) = self.peek() {
            if expected.matches(&t.token_type) {
                self.advance();
                return true;
            }
        }
        return false;
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

    fn advance(&mut self) -> Option<&Token> {
        if self.is_consumed() {
            None
        } else {
            self.current_idx += 1;
            Some(&self.tokens[self.current_idx])
        }
    }

    fn is_consumed(&self) -> bool {
        self.is_consumed_ahead(0)
    }

    fn is_peekable(&self) -> bool {
        !self.is_consumed_ahead(1)
    }

    fn is_consumed_ahead(&self, delta: usize) -> bool {
        self.current_idx + delta >= self.tokens.len()
    }

    fn take_expr(&mut self) -> Result<Box<Expr>, ParseError> {
        match self.expr.take() {
            Some(v) => Ok(v),
            None => Err(ParseError::InternalError {
                msg: format!("{}", "expected expr to be populated"),
            }),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Box<Expr>, ParseError> {
    // TODO: Add error handling/parser synchronization
    let mut state = ParserState::new(tokens);
    state = expression(state)?;
    match state.expr {
        Some(e) => Ok(e),
        None => Err(ParseError::InternalError {
            msg: format!("{}", "Failed to produce expression"),
        }),
    }
}

fn expression(state: ParserState) -> Result<ParserState, ParseError> {
    equality(state)
}

fn equality(mut state: ParserState) -> Result<ParserState, ParseError> {
    state = comparison(state)?;
    let mut expr = state.take_expr()?;
    while state.match_advance2(TokenType::BangEqual, TokenType::EqualEqual) {
        let operator: Box<Op> = Box::new(state.previous()?.into());
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
        let operator: Box<Op> = Box::new(state.previous()?.into());
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
        let operator: Box<Op> = Box::new(state.previous()?.into());
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
        let operator: Box<Op> = Box::new(state.previous()?.into());
        state = unary(state)?;
        let right = state.take_expr()?;
        expr = Box::new(Expr::Binary(expr, operator, right));
    }
    state.expr = Some(expr);
    Ok(state)
}

fn unary(mut state: ParserState) -> Result<ParserState, ParseError> {
    if state.match_advance2(TokenType::Bang, TokenType::Minus) {
        let operator: Box<Op> = Box::new(state.previous()?.into());
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
    if state.match_advance(TokenType::False) {
        state.expr = Some(Box::new(Expr::BooleanLiteral(false)));
    }
    if state.match_advance(TokenType::True) {
        state.expr = Some(Box::new(Expr::BooleanLiteral(true)));
    }
    if state.match_advance(TokenType::Nil) {
        state.expr = Some(Box::new(Expr::NilLiteral));
    }
    if state.match_advance(TokenType::Str) {
        let t = state.previous()?;
        state.expr = Some(Box::new(Expr::StringLiteral(t.lexeme.to_string())));
    }
    if state.match_advance(TokenType::Numeric) {
        let t = state.previous()?;
        let num = match t.lexeme.parse::<f64>() {
            Ok(v) => v,
            Err(why) => {
                return Err(ParseError::Panic {
                    line: t.line,
                    lexeme: t.lexeme.to_string(),
                    msg: format!("{}: {:#?}", "Failed to parse numeric", why),
                })
            }
        };
        state.expr = Some(Box::new(Expr::NumericLiteral(num)));
    }
    if state.match_advance(TokenType::LeftParen) {
        state = expression(state)?;
        // Ensure the parentheses are closed
        state.consume(&TokenType::RightParen, "Expect ')' after expression")?;
        state.expr = Some(Box::new(Expr::Grouping(state.take_expr()?)));
    }
    if let None = state.expr {
        Err(ParseError::InternalError {
            msg: format!("Unsupported token: {:?}", state.current()?),
        })
    } else {
        Ok(state)
    }
}
