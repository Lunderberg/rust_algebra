#![allow(dead_code)]

use typed_dag::RefType;
use typed_dag_derive::typed_dag;

#[typed_dag]
pub mod expr {
    pub enum Numeric {
        Int(i64),
        Float(f64),
        Add(Numeric, Numeric),
        Sub(Numeric, Numeric),
        Mul(Numeric, Numeric),
        Div(Numeric, Numeric),
    }

    pub enum Boolean {
        Bool(bool),
        Equal(Numeric, Numeric),
        And(Boolean, Boolean),
        Or(Boolean, Boolean),
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

impl<R: RefType<'static>> BinaryOperator for expr::Numeric<R> {
    fn precedence(&self) -> Option<(OperatorPrecedence, &str)> {
        match self {
            expr::Numeric::Int(_) => None,
            expr::Numeric::Float(_) => None,
            expr::Numeric::Add(_, _) => Some((OperatorPrecedence::AddSub, " + ")),
            expr::Numeric::Sub(_, _) => Some((OperatorPrecedence::AddSub, " - ")),
            expr::Numeric::Mul(_, _) => Some((OperatorPrecedence::MulDiv, "*")),
            expr::Numeric::Div(_, _) => Some((OperatorPrecedence::MulDiv, "/")),
        }
    }
}
