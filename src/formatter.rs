use std::fmt::{Display, Formatter};

use algebra::{expr, BinaryOperator};
use typed_dag::Visitable;

impl<'view, V: expr::visitor::Numeric<'view>> Display for expr::Numeric<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            expr::Numeric::Int(val) => write!(f, "{val}"),
            expr::Numeric::Float(val) => write!(f, "{val}"),
            expr::Numeric::Add(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            expr::Numeric::Sub(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            expr::Numeric::Mul(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            expr::Numeric::Div(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
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
