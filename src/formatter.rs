use std::fmt::{Debug, Display, Formatter};

use algebra::expr::{visitor, BinaryOperator, Expr};
use typed_dag::Visitable;

impl<'view, V: visitor::Expr<'view>> Debug for Expr<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(arg0) => f.debug_tuple("Int").field(arg0).finish(),
            Self::Float(arg0) => f.debug_tuple("Float").field(arg0).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::Add(_, _) => f.debug_tuple("Add").finish(),
            Self::Sub(_, _) => f.debug_tuple("Sub").finish(),
            Self::Mul(_, _) => f.debug_tuple("Mul").finish(),
            Self::Div(_, _) => f.debug_tuple("Div").finish(),
            Self::Equal(_, _) => f.debug_tuple("Equal").finish(),
            Self::And(_, _) => f.debug_tuple("And").finish(),
            Self::Or(_, _) => f.debug_tuple("Or").finish(),
            Self::UnaryNeg(_) => f.debug_tuple("UnaryNeg").finish(),
            Self::UnaryNot(_) => f.debug_tuple("UnaryNot").finish(),
        }
    }
}

impl<'view, V: visitor::Expr<'view>> Display for Expr<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int(val) => write!(f, "{val}"),
            Expr::Float(val) => write!(f, "{val}"),
            Expr::Bool(val) => write!(f, "{val}"),
            Expr::Add(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Expr::Sub(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Expr::Mul(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Expr::Div(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Expr::Equal(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Expr::And(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Expr::Or(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Expr::UnaryNeg(arg) => display_unary_op(self, f, arg.borrow()),
            Expr::UnaryNot(arg) => display_unary_op(self, f, arg.borrow()),
        }
    }
}

fn display_unary_op<Arg: Display + BinaryOperator>(
    op: &impl BinaryOperator,
    f: &mut std::fmt::Formatter<'_>,
    arg: Arg,
) -> std::fmt::Result {
    let precedence = op.precedence().unwrap();
    let arg_precedence = arg.precedence().unwrap();

    let display_str = op.display_str().unwrap();

    if arg_precedence > precedence {
        write!(f, "{display_str}{arg}")
    } else {
        write!(f, "{display_str}({arg})")
    }
}

fn display_binary_op<LHS: Display + BinaryOperator, RHS: Display + BinaryOperator>(
    op: &impl BinaryOperator,
    f: &mut std::fmt::Formatter<'_>,
    lhs: LHS,
    rhs: RHS,
) -> std::fmt::Result {
    let precedence = op.precedence().unwrap();

    let lhs_paren = lhs.precedence().map(|p| precedence > p).unwrap_or(false);
    let rhs_paren = rhs.precedence().map(|p| precedence >= p).unwrap_or(false);

    if lhs_paren {
        write!(f, "({lhs})")?;
    } else {
        write!(f, "{lhs}")?;
    }

    write!(f, "{}", op.display_str().unwrap())?;

    if rhs_paren {
        write!(f, "({rhs})")?;
    } else {
        write!(f, "{rhs}")?;
    }

    Ok(())
}
