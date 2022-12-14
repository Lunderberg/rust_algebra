use thiserror::Error;

use crate::Token;

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
    #[error("GraphError: {0}")]
    GraphError(graph::Error),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<graph::Error> for Error {
    fn from(value: graph::Error) -> Self {
        Error::GraphError(value)
    }
}
