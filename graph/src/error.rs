use thiserror::Error;

#[derive(Error, Clone)]
pub enum Error {
    #[error("Invalid reference {rel_pos} in subgraph of size {subgraph_size}")]
    InvalidReference {
        rel_pos: usize,
        subgraph_size: usize,
    },
    #[error("Expected type {expected}, but received {actual}")]
    IncorrectType {
        expected: &'static str,
        actual: &'static str,
    },
    #[error("Empty expression")]
    EmptyExpression,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
