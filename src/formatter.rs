use std::fmt::{Display, Formatter};

use algebra::{expr, BinaryOperator};
use graph::{ContainerOf, Visiting};

impl<'a, Container: ContainerOf<expr::IntExpr<'a>>> Display
    for expr::IntExpr<'a, Visiting<'a, Container>>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            expr::IntExpr::Int(val) => write!(f, "{val}"),
            expr::IntExpr::Add(lhs, rhs) => {
                display_binary_op(self, f, lhs.borrow().unwrap(), rhs.borrow().unwrap())
            }
            expr::IntExpr::Sub(lhs, rhs) => {
                display_binary_op(self, f, lhs.borrow().unwrap(), rhs.borrow().unwrap())
            }
            expr::IntExpr::Mul(lhs, rhs) => {
                display_binary_op(self, f, lhs.borrow().unwrap(), rhs.borrow().unwrap())
            }
            expr::IntExpr::Div(lhs, rhs) => {
                display_binary_op(self, f, lhs.borrow().unwrap(), rhs.borrow().unwrap())
            }
        }
    }
}

fn display_binary_op<LHS: Display + BinaryOperator, RHS: Display + BinaryOperator>(
    op: &impl BinaryOperator,
    f: &mut std::fmt::Formatter<'_>,
    lhs: LHS,
    rhs: RHS,
) -> std::fmt::Result {
    let (precedence, op_str) = op
        .precedence()
        .expect("display_binary_op() called for something other than a binary op");

    let lhs_precedence = lhs.precedence();
    let rhs_precedence = rhs.precedence();

    if lhs_precedence.map_or(true, |(p, _)| p >= precedence) {
        write!(f, "{lhs}")?;
    } else {
        write!(f, "({lhs})")?;
    }

    write!(f, "{op_str}")?;

    if rhs_precedence.map_or(true, |(p, _)| p > precedence) {
        write!(f, "{rhs}")?;
    } else {
        write!(f, "({rhs})")?;
    }

    Ok(())
}
