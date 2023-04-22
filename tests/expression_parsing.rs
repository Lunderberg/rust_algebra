use algebra::{parse_expr, Bool, Error, Expr, Int, Rational};
use typed_dag::{Arena, Visitable};
use typed_dag_derive::tree;

type Container = <algebra::Expr as typed_dag::HasDefaultContainer>::Container;

fn parse_compare(string: &str, expected: Arena<Container, Expr>) -> Result<(), Error> {
    let parsed = Arena::try_build(|arena| parse_expr(string.chars(), arena))?;
    // println!("Parsed: {}", parsed.visit_root().borrow());
    // println!("Expected: {}", expected.visit_root().borrow());
    assert_eq!(parsed.visit_root().borrow(), expected.visit_root().borrow());
    Ok(())
}

#[test]
fn test_parse_one_digit_int() -> Result<(), Error> {
    parse_compare("5", tree! {Expr::Int(Int::Literal(5))})
}

#[test]
fn test_parse_two_digit_int() -> Result<(), Error> {
    parse_compare("42", tree! {Expr::Int(Int::Literal(42))})
}

#[test]
fn test_parse_negative_int() -> Result<(), Error> {
    parse_compare("-12", tree! {Expr::Int(Int::Literal(-12))})
}

#[test]
fn test_parse_addition() -> Result<(), Error> {
    parse_compare(
        "5+10",
        tree! {Expr::Int(Int::Add(Int::Literal(5), Int::Literal(10)))},
    )
}

#[test]
fn test_parse_addition_with_spaces() -> Result<(), Error> {
    parse_compare(
        "5 + 10",
        tree! {Expr::Int(Int::Add(Int::Literal(5), Int::Literal(10)))},
    )
}

#[test]
fn test_parse_multiple_addition() -> Result<(), Error> {
    parse_compare(
        "5 + 10 + 15",
        tree! {Expr::Int(Int::Add(Int::Add(Int::Literal(5),Int::Literal(10)), Int::Literal(15)))},
    )
}

#[test]
fn test_parse_subtraction() -> Result<(), Error> {
    parse_compare(
        "5 - 10",
        tree! {Expr::Int(Int::Sub(Int::Literal(5), Int::Literal(10)))},
    )
}

#[test]
fn test_parse_mixed_addition_subtraction() -> Result<(), Error> {
    parse_compare(
        "5 + 15 - 10",
        tree! {Expr::Int(Int::Sub(Int::Add(Int::Literal(5), Int::Literal(15)), Int::Literal(10)))},
    )
}

#[test]
fn test_parse_parentheses() -> Result<(), Error> {
    parse_compare(
        "5 + (15 - 10)",
        tree! {Expr::Int(Int::Add(Int::Literal(5), Int::Sub(Int::Literal(15), Int::Literal(10))))},
    )
}

#[test]
fn test_parse_multiply() -> Result<(), Error> {
    parse_compare(
        "5 * 10",
        tree! {Expr::Int(Int::Mul(Int::Literal(5), Int::Literal(10)))},
    )
}

#[test]
fn test_parse_multiply_left_precedence() -> Result<(), Error> {
    parse_compare(
        "5*10 + 15",
        tree! {Expr::Int(Int::Add(Int::Mul(Int::Literal(5),Int::Literal(10)), Int::Literal(15)))},
    )
}

#[test]
fn test_parse_multiply_right_precedence() -> Result<(), Error> {
    parse_compare(
        "5 + 10*15",
        tree! {Expr::Int(Int::Add(Int::Literal(5), Int::Mul(Int::Literal(10),Int::Literal(15))))},
    )
}

#[test]
fn test_parse_divide() -> Result<(), Error> {
    parse_compare(
        "5 / 10",
        tree! {Expr::Rational(Rational::Ratio(Int::Literal(5), Int::Literal(10)))},
    )
}

#[test]
fn test_parse_mixed_multiply_divide() -> Result<(), Error> {
    parse_compare(
        "2 * 5 / 10 * 42",
        tree! {Expr::Rational(Rational::Mul(
        Rational::Ratio(Int::Mul(Int::Literal(2), Int::Literal(5)),
            Int::Literal(10)),
           Rational::Int(Int::Literal(42))))},
    )
}

#[test]
fn test_literal_true() -> Result<(), Error> {
    parse_compare("true", tree! {Expr::Bool(Bool::Literal(true))})
}

#[test]
fn test_literal_false() -> Result<(), Error> {
    parse_compare("false", tree! {Expr::Bool(Bool::Literal(false))})
}

#[test]
fn test_positive_float() -> Result<(), Error> {
    parse_compare(
        "1.5",
        tree! {Expr::Rational(Rational::Ratio(Int::Literal(3), Int::Literal(2)))},
    )
}

#[test]
fn test_negative_float() -> Result<(), Error> {
    parse_compare(
        "-1.5",
        tree! {Expr::Rational(Rational::Ratio(Int::Literal(-3), Int::Literal(2)))},
    )
}

#[test]
fn test_positive_float_without_ipart() -> Result<(), Error> {
    parse_compare(
        ".5",
        tree! {Expr::Rational(Rational::Ratio(Int::Literal(1), Int::Literal(2)))},
    )
}

#[test]
fn test_negative_float_without_ipart() -> Result<(), Error> {
    parse_compare(
        "-.5",
        tree! {Expr::Rational(Rational::Ratio(Int::Literal(-1), Int::Literal(2)))},
    )
}

#[test]
fn test_positive_float_without_fpart() -> Result<(), Error> {
    parse_compare("1.", tree! {Expr::Int(Int::Literal(1))})
}

#[test]
fn test_negative_float_without_fpart() -> Result<(), Error> {
    parse_compare("-1.", tree! {Expr::Int(Int::Literal(-1))})
}

#[test]
fn test_float_with_leading_zero_in_fpart() -> Result<(), Error> {
    parse_compare(
        "1.0625",
        tree! {Expr::Rational(Rational::Ratio(Int::Literal(17), Int::Literal(16)))},
    )
}

#[test]
fn test_standalone_dot_is_error() -> Result<(), Error> {
    assert!(Arena::try_build(|arena| parse_expr(".".chars(), arena)).is_err());
    Ok(())
}

#[test]
fn test_boolean_and() -> Result<(), Error> {
    parse_compare(
        "true && false",
        tree! {Expr::Bool(Bool::And(Bool::Literal(true), Bool::Literal(false)))},
    )
}

#[test]
fn test_boolean_or() -> Result<(), Error> {
    parse_compare(
        "false || true",
        tree! {Expr::Bool(Bool::Or(Bool::Literal(false), Bool::Literal(true)))},
    )
}
