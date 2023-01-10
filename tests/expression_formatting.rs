use algebra::expr::IntExpr;
use graph::{Builder, TypedTree};
use graph_derive::tree;

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
    let expr: TypedTree<IntExpr> = tree![IntExpr::Add(IntExpr::Int(5), IntExpr::Int(10))];
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
