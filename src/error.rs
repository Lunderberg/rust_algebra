use thiserror::Error;

use crate::Token;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Copy, Clone)]
pub enum Error {
    #[error("Parse error")]
    ParseError,
    #[error("Unexpected character: '{0}'")]
    UnexpectedCharacter(char),
    #[error("Unexpected end of expr")]
    UnexpectedEndOfExpr,
    #[error("Unexpected token: {0:?}")]
    UnexpectedToken(Token),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
