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

#[cfg(test)]
mod test {
    use algebra::expr::*;
    use graph::{Builder, TypedTree};

    #[test]
    fn format_int() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            builder.push(IntExpr::Int(5));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5");
    }

    #[test]
    fn format_add() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            builder.push(IntExpr::Add(a, b));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5 + 10");
    }

    #[test]
    fn format_sub() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            builder.push(IntExpr::Sub(a, b));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5 - 10");
    }

    #[test]
    fn format_left_associative_add() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            let c = builder.push(IntExpr::Int(15));
            let d = builder.push(IntExpr::Add(a, b));
            builder.push(IntExpr::Add(d, c));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5 + 10 + 15");
    }

    #[test]
    fn format_right_associative_add() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            let c = builder.push(IntExpr::Int(15));
            let d = builder.push(IntExpr::Add(b, c));
            builder.push(IntExpr::Add(a, d));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5 + (10 + 15)");
    }

    #[test]
    fn format_mul() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            builder.push(IntExpr::Mul(a, b));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5*10");
    }

    #[test]
    fn format_div() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            builder.push(IntExpr::Div(a, b));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5/10");
    }

    #[test]
    fn format_left_associative_mul() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            let c = builder.push(IntExpr::Int(15));
            let d = builder.push(IntExpr::Mul(a, b));
            builder.push(IntExpr::Mul(d, c));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5*10*15");
    }

    #[test]
    fn format_right_associative_mul() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            let c = builder.push(IntExpr::Int(15));
            let d = builder.push(IntExpr::Mul(b, c));
            builder.push(IntExpr::Mul(a, d));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5*(10*15)");
    }

    #[test]
    fn format_mul_lhs_add() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            let c = builder.push(IntExpr::Int(15));
            let d = builder.push(IntExpr::Add(a, b));
            builder.push(IntExpr::Mul(d, c));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "(5 + 10)*15");
    }

    #[test]
    fn format_mul_rhs_add() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            let c = builder.push(IntExpr::Int(15));
            let d = builder.push(IntExpr::Add(b, c));
            builder.push(IntExpr::Mul(a, d));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5*(10 + 15)");
    }

    #[test]
    fn format_add_lhs_mul() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            let c = builder.push(IntExpr::Int(15));
            let d = builder.push(IntExpr::Mul(a, b));
            builder.push(IntExpr::Add(d, c));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5*10 + 15");
    }

    #[test]
    fn format_add_rhs_mul() {
        let expr: TypedTree<IntExpr> = {
            let mut builder = Builder::new();
            let a = builder.push(IntExpr::Int(5));
            let b = builder.push(IntExpr::Int(10));
            let c = builder.push(IntExpr::Int(15));
            let d = builder.push(IntExpr::Mul(b, c));
            builder.push(IntExpr::Add(a, d));
            builder.into()
        };
        let formatted = format!("{}", expr.root());
        assert_eq!(formatted, "5 + 10*15");
    }
}
