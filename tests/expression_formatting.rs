use algebra::{Bool, Int, Rational};
use typed_dag::Visitable;
use typed_dag_derive::tree;

#[test]
fn format_int() {
    let expr = tree![Int::Literal(5)];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5");
}

#[test]
fn format_add() {
    let expr = tree![Int::Add(Int::Literal(5), Int::Literal(10))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 + 10");
}

#[test]
fn format_sub() {
    let expr = tree![Int::Sub(Int::Literal(5), Int::Literal(10))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 - 10");
}

#[test]
fn format_left_associative_add() {
    let expr = tree![Int::Add(
        Int::Add(Int::Literal(5), Int::Literal(10)),
        Int::Literal(15)
    )];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 + 10 + 15");
}

#[test]
fn format_right_associative_add() {
    let expr = tree![Int::Add(
        Int::Literal(5),
        Int::Add(Int::Literal(10), Int::Literal(15))
    )];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 + (10 + 15)");
}

#[test]
fn format_mul() {
    let expr = tree![Int::Mul(Int::Literal(5), Int::Literal(10))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*10");
}

#[test]
fn format_div() {
    let expr = tree![Rational::Ratio(Int::Literal(5), Int::Literal(10))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5/10");
}

#[test]
fn format_left_associative_mul() {
    let expr = tree![Int::Mul(
        Int::Mul(Int::Literal(5), Int::Literal(10)),
        Int::Literal(15)
    )];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*10*15");
}

#[test]
fn format_right_associative_mul() {
    let expr = tree![Int::Mul(
        Int::Literal(5),
        Int::Mul(Int::Literal(10), Int::Literal(15))
    )];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*(10*15)");
}

#[test]
fn format_mul_lhs_add() {
    let expr = tree![Int::Mul(
        Int::Add(Int::Literal(5), Int::Literal(10)),
        Int::Literal(15)
    )];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "(5 + 10)*15");
}

#[test]
fn format_mul_rhs_add() {
    let expr = tree![Int::Mul(
        Int::Literal(5),
        Int::Add(Int::Literal(10), Int::Literal(15))
    )];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*(10 + 15)");
}

#[test]
fn format_add_lhs_mul() {
    let expr = tree![Int::Add(
        Int::Mul(Int::Literal(5), Int::Literal(10)),
        Int::Literal(15)
    )];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*10 + 15");
}

#[test]
fn format_add_rhs_mul() {
    let expr = tree![Int::Add(
        Int::Literal(5),
        Int::Mul(Int::Literal(10), Int::Literal(15))
    )];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 + 10*15");
}

#[test]
fn format_true() {
    let expr = tree![Bool::Literal(true)];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "true");
}

#[test]
fn format_false() {
    let expr = tree![Bool::Literal(false)];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "false");
}
