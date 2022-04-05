use thiserror::Error;
use crate::lib::scanning::Token;

#[derive(Error, Debug)]
pub enum LoxError {
    #[error("Runtime error at {at:?}: {message}")]
    Runtime {
        message: String,
        at: Token,
    }
}