use crate::lib::scanning::{Token, TokenType};

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Box<Op>, Box<Expr>),
    Grouping(Box<Expr>),
    StringLiteral(String),
    NumericLiteral(f64),
    BooleanLiteral(bool),
    NilLiteral,
    Unary(Box<Op>, Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Op {
    pub token_type: TokenType,
}

impl From<&Token> for Op {
    fn from(item: &Token) -> Self {
        let token_type = item.token_type.clone();
        Op { token_type }
    }
}

pub trait ExprVisitor<T> {
    fn visit_expr(&mut self, e: &Expr) -> T;
}
