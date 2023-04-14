use algebra::expr::Numeric::*;
use typed_dag::Visitable;
use typed_dag_derive::tree;

#[test]
fn format_int() {
    let expr = tree![Int(5)];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5");
}

#[test]
fn format_add() {
    let expr = tree![Add(Int(5), Int(10))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 + 10");
}

#[test]
fn format_sub() {
    let expr = tree![Sub(Int(5), Int(10))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 - 10");
}

#[test]
fn format_left_associative_add() {
    let expr = tree![Add(Add(Int(5), Int(10)), Int(15))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 + 10 + 15");
}

#[test]
fn format_right_associative_add() {
    let expr = tree![Add(Int(5), Add(Int(10), Int(15)))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 + (10 + 15)");
}

#[test]
fn format_mul() {
    let expr = tree![Mul(Int(5), Int(10))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*10");
}

#[test]
fn format_div() {
    let expr = tree![Div(Int(5), Int(10))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5/10");
}

#[test]
fn format_left_associative_mul() {
    let expr = tree![Mul(Mul(Int(5), Int(10)), Int(15))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*10*15");
}

#[test]
fn format_right_associative_mul() {
    let expr = tree![Mul(Int(5), Mul(Int(10), Int(15)))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*(10*15)");
}

#[test]
fn format_mul_lhs_add() {
    let expr = tree![Mul(Add(Int(5), Int(10)), Int(15))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "(5 + 10)*15");
}

#[test]
fn format_mul_rhs_add() {
    let expr = tree![Mul(Int(5), Add(Int(10), Int(15)))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*(10 + 15)");
}

#[test]
fn format_add_lhs_mul() {
    let expr = tree![Add(Mul(Int(5), Int(10)), Int(15))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5*10 + 15");
}

#[test]
fn format_add_rhs_mul() {
    let expr = tree![Add(Int(5), Mul(Int(10), Int(15)))];
    let formatted = format!("{}", expr.visit_root().borrow());
    assert_eq!(formatted, "5 + 10*15");
}
