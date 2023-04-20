#![allow(dead_code)]

use typed_dag::RefType;
use typed_dag_derive::typed_dag;

#[typed_dag]
mod expr {
    pub enum Expr {
        Int(i64),
        Bool(bool),
        UnaryNeg(Expr),
        UnaryNot(Expr),
        Add(Expr, Expr),
        Sub(Expr, Expr),
        Mul(Expr, Expr),
        Div(Expr, Expr),
        Equal(Expr, Expr),
        And(Expr, Expr),
        Or(Expr, Expr),
    }
}
pub use expr::*;

#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub(crate) enum OperatorPrecedence {
    Expr,
    UnaryNeg,
    AddSub,
    MulDiv,
    Comparison,
    Boolean,
}

pub(crate) trait BinaryOperator {
    fn precedence(&self) -> Option<OperatorPrecedence>;
    fn display_str(&self) -> Option<&str>;
}

impl<R: RefType<'static>> BinaryOperator for expr::Expr<R> {
    fn precedence(&self) -> Option<OperatorPrecedence> {
        match self {
            expr::Expr::Int(_) => None,
            expr::Expr::Bool(_) => None,
            expr::Expr::UnaryNeg(_) => Some(OperatorPrecedence::UnaryNeg),
            expr::Expr::Add(_, _) => Some(OperatorPrecedence::AddSub),
            expr::Expr::Sub(_, _) => Some(OperatorPrecedence::AddSub),
            expr::Expr::Mul(_, _) => Some(OperatorPrecedence::MulDiv),
            expr::Expr::Div(_, _) => Some(OperatorPrecedence::MulDiv),
            expr::Expr::Equal(_, _) => Some(OperatorPrecedence::Comparison),
            expr::Expr::And(_, _) => Some(OperatorPrecedence::Boolean),
            expr::Expr::Or(_, _) => Some(OperatorPrecedence::Boolean),
            expr::Expr::UnaryNot(_) => Some(OperatorPrecedence::Boolean),
        }
    }

    fn display_str(&self) -> Option<&str> {
        match self {
            expr::Expr::Int(_) => None,
            expr::Expr::Bool(_) => None,
            expr::Expr::UnaryNeg(_) => Some("-"),
            expr::Expr::Add(_, _) => Some(" + "),
            expr::Expr::Sub(_, _) => Some(" - "),
            expr::Expr::Mul(_, _) => Some("*"),
            expr::Expr::Div(_, _) => Some("/"),
            expr::Expr::Equal(_, _) => Some(" == "),
            expr::Expr::And(_, _) => Some(" && "),
            expr::Expr::Or(_, _) => Some(" || "),
            expr::Expr::UnaryNot(_) => Some("!"),
        }
    }
}
