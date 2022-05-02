use crate::lib::err::LoxError;
use crate::lib::scanner::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(LiteralValue),
    Unary(Token, Box<Expr>),
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Expr {
    pub fn accept<T>(&self, visitor: &mut dyn ExprVisitor<T>) -> Result<T, LoxError> {
        match self {
            Expr::Binary(left, operator, right) => visitor.visit_binary_expr(left, operator, right),
            Expr::Grouping(expression) => visitor.visit_grouping_expr(expression),
            Expr::Literal(literal_value) => visitor.visit_literal_expr(literal_value),
            Expr::Unary(operator, right) => visitor.visit_unary_expr(operator, right),
        }
    }
}

pub trait ExprVisitor<T> {
    fn visit_binary_expr(
        &mut self,
        left: &Expr,
        operator: &Token,
        right: &Expr,
    ) -> Result<T, LoxError>;
    fn visit_grouping_expr(&mut self, expression: &Expr) -> Result<T, LoxError>;
    fn visit_literal_expr(&mut self, value: &LiteralValue) -> Result<T, LoxError>;
    fn visit_unary_expr(&mut self, operator: &Token, right: &Expr) -> Result<T, LoxError>;
}

#[derive(Debug)]
pub enum Stmt {
    Expression(Box<Expr>),
    Print(Box<Expr>),
    If(Box<Expr>, Box<Stmt>, Option<Box<Stmt>>),
}

impl Stmt {
    pub fn accept<T>(&self, visitor: &mut dyn StmtVisitor<T>) -> Result<T, LoxError> {
        match self {
            Stmt::Expression(expression) => visitor.visit_expression_stmt(expression),
            Stmt::Print(expression) => visitor.visit_print_stmt(expression),
            Stmt::If(condition, then_branch, else_branch) => {
                visitor.visit_if_stmt(condition, then_branch, else_branch.as_deref())
            }
        }
    }
}

pub trait StmtVisitor<T> {
    fn visit_expression_stmt(&mut self, expression: &Expr) -> Result<T, LoxError>;
    fn visit_print_stmt(&mut self, expression: &Expr) -> Result<T, LoxError>;
    fn visit_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: Option<&Stmt>,
    ) -> Result<T, LoxError>;
}
