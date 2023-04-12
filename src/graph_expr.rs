#![allow(dead_code)]

use graph::RefType;
use graph_derive::recursive_graph;

#[recursive_graph]
pub mod expr {
    // #[derive(Debug)]
    pub enum IntExpr {
        Int(i64),
        Add(IntExpr, IntExpr),
        Sub(IntExpr, IntExpr),
        Mul(IntExpr, IntExpr),
        Div(IntExpr, IntExpr),
    }

    // #[derive(Debug)]
    pub enum FloatExpr {
        Float(f64),
        Add(FloatExpr, FloatExpr),
        Sub(FloatExpr, FloatExpr),
    }

    // #[derive(Debug)]
    pub enum BoolExpr {
        Bool(bool),
        IntEqual(IntExpr, IntExpr),
        FloatEqual(FloatExpr, FloatExpr),
        And(BoolExpr, BoolExpr),
        Or(BoolExpr, BoolExpr),
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub(crate) enum OperatorPrecedence {
    Expr,
    AddSub,
    MulDiv,
}

pub(crate) trait BinaryOperator {
    fn precedence(&self) -> Option<(OperatorPrecedence, &str)>;
}

impl<'a, R: RefType> BinaryOperator for expr::IntExpr<'a, R> {
    fn precedence(&self) -> Option<(OperatorPrecedence, &str)> {
        match self {
            expr::IntExpr::Int(_) => None,
            expr::IntExpr::Add(_, _) => Some((OperatorPrecedence::AddSub, " + ")),
            expr::IntExpr::Sub(_, _) => Some((OperatorPrecedence::AddSub, " - ")),
            expr::IntExpr::Mul(_, _) => Some((OperatorPrecedence::MulDiv, "*")),
            expr::IntExpr::Div(_, _) => Some((OperatorPrecedence::MulDiv, "/")),
        }
    }
}
