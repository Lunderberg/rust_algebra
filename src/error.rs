use thiserror::Error;

use crate::Token;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Clone)]
pub enum Error {
    #[error("Parse error")]
    ParseError,
    #[error("Unexpected character: '{0}'")]
    UnexpectedCharacter(char),
    #[error("Unexpected end of expr")]
    UnexpectedEndOfExpr,
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(Token),
    #[error("Not yet implemented: {0}")]
    NotImplemented(String),
    #[error("Invalid reference {rel_pos} in subgraph of size {subgraph_size}")]
    InvalidReference {
        rel_pos: usize,
        subgraph_size: usize,
    },
    #[error("Expected type {expected}, but received {actual}")]
    IncorrectType { expected: String, actual: String },
    #[error("Empty expression")]
    EmptyExpression,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
