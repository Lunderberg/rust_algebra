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
    #[error("Expected start of expression, but found token {0:?}")]
    ExpectedStartOfExpr(Token),
    #[error("Expected token {0:?}, but found token {1:?}")]
    UnexpectedToken(Token, Token),
    #[error("Expected end of file, but found token: {0:?}")]
    ExpectedEndOfFile(Token),
    #[error("Not yet implemented: {0}")]
    NotImplemented(String),
    #[error("GraphError: {0}")]
    GraphError(typed_dag::Error),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<typed_dag::Error> for Error {
    fn from(value: typed_dag::Error) -> Self {
        Error::GraphError(value)
    }
}
