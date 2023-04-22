use std::fmt::{Debug, Display, Formatter};

use algebra::expr::{visitor, BinaryOperator, Bool, Expr, Int, Rational};
use typed_dag::Visitable;

impl<'view, V: visitor::Expr<'view>> Debug for Expr<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(arg0) => f.debug_tuple("Int").field(&arg0.borrow()).finish(),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(&arg0.borrow()).finish(),
            Self::Rational(arg0) => f.debug_tuple("Rational").field(&arg0.borrow()).finish(),
        }
    }
}

impl<'view, V: visitor::Expr<'view>> Display for Expr<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Int(val) => write!(f, "{}", val.borrow()),
            Expr::Bool(val) => write!(f, "{}", val.borrow()),
            Expr::Rational(val) => write!(f, "{}", val.borrow()),
        }
    }
}

impl<'view, V: visitor::Int<'view>> Debug for Int<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(arg0) => f.debug_tuple("Literal").field(arg0).finish(),
            Self::Add(arg0, arg1) => f
                .debug_tuple("Add")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::Sub(arg0, arg1) => f
                .debug_tuple("Sub")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::Mul(arg0, arg1) => f
                .debug_tuple("Mul")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::FloorDiv(arg0, arg1) => f
                .debug_tuple("FloorDiv")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::FloorMod(arg0, arg1) => f
                .debug_tuple("FloorMod")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::Negative(arg0) => f.debug_tuple("Negative").field(&arg0.borrow()).finish(),
        }
    }
}

impl<'view, V: visitor::Int<'view>> Display for Int<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Int::Literal(val) => write!(f, "{val}"),
            Int::Add(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Int::Sub(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Int::Mul(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Int::FloorDiv(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Int::FloorMod(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Int::Negative(arg) => display_unary_op(self, f, arg.borrow()),
        }
    }
}

impl<'view, V: visitor::Bool<'view>> Debug for Bool<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(arg0) => f.debug_tuple("Literal").field(arg0).finish(),
            Self::IntEqual(arg0, arg1) => f
                .debug_tuple("IntEqual")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::RationalEqual(arg0, arg1) => f
                .debug_tuple("RationalEqual")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::Not(arg0) => f.debug_tuple("Not").field(&arg0.borrow()).finish(),
            Self::And(arg0, arg1) => f
                .debug_tuple("And")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::Or(arg0, arg1) => f
                .debug_tuple("Or")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
        }
    }
}

impl<'view, V: visitor::Bool<'view>> Display for Bool<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Bool::Literal(val) => write!(f, "{val}"),
            Bool::And(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Bool::Or(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Bool::Not(arg) => display_unary_op(self, f, arg.borrow()),
            Bool::IntEqual(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Bool::RationalEqual(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
        }
    }
}

impl<'view, V: visitor::Rational<'view>> Debug for Rational<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(arg0) => f.debug_tuple("Int").field(&arg0.borrow()).finish(),
            Self::Ratio(arg0, arg1) => f
                .debug_tuple("Ratio")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::Negative(arg0) => f.debug_tuple("Negative").field(&arg0.borrow()).finish(),
            Self::Add(arg0, arg1) => f
                .debug_tuple("Add")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::Sub(arg0, arg1) => f
                .debug_tuple("Sub")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::Mul(arg0, arg1) => f
                .debug_tuple("Mul")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
            Self::Div(arg0, arg1) => f
                .debug_tuple("Div")
                .field(&arg0.borrow())
                .field(&arg1.borrow())
                .finish(),
        }
    }
}

impl<'view, V: visitor::Rational<'view>> Display for Rational<V> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Rational::Int(arg) => write!(f, "{}", arg.borrow()),
            Rational::Ratio(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Rational::Add(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Rational::Sub(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Rational::Mul(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Rational::Div(lhs, rhs) => display_binary_op(self, f, lhs.borrow(), rhs.borrow()),
            Rational::Negative(arg) => display_unary_op(self, f, arg.borrow()),
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
