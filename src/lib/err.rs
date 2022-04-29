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

    #[error("Internal error: {message}")]
    Internal {
        message: String,
    },
}