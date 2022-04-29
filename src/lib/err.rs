use thiserror::Error;
use crate::lib::scanner::Token;

#[derive(Error, Debug)]
pub enum LoxError {
    #[error("Runtime error at {at:?}: {message}")]
    Runtime {
        message: String,
        at: Token,
    },

    #[error("Parse error on line {line} at '{lexeme}': {message}")]
    Parse {
        line: usize,
        lexeme: String,
        message: String,
    },

    #[error("Syntax error on line {line} at position {line_pos}: {message}")]
    Syntax {
        line: usize,
        line_pos: usize,
        message: String,
    },

    #[error("Internal error: {message}")]
    Internal {
        message: String,
    },
}