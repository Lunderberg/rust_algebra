use algebra::{parse_expr, Error, Expr, Expr::*};
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
    parse_compare("5", tree! {Int(5)})
}

#[test]
fn test_parse_two_digit_int() -> Result<(), Error> {
    parse_compare("42", tree! {Int(42)})
}

#[test]
fn test_parse_negative_int() -> Result<(), Error> {
    parse_compare("-12", tree! {Int(-12)})
}

#[test]
fn test_parse_addition() -> Result<(), Error> {
    parse_compare("5+10", tree! {Add(Int(5), Int(10))})
}

#[test]
fn test_parse_addition_with_spaces() -> Result<(), Error> {
    parse_compare("5 + 10", tree! {Add(Int(5), Int(10))})
}

#[test]
fn test_parse_multiple_addition() -> Result<(), Error> {
    parse_compare("5 + 10 + 15", tree! {Add(Add(Int(5),Int(10)), Int(15))})
}

#[test]
fn test_parse_subtraction() -> Result<(), Error> {
    parse_compare("5 - 10", tree! {Sub(Int(5), Int(10))})
}

#[test]
fn test_parse_mixed_addition_subtraction() -> Result<(), Error> {
    parse_compare("5 + 15 - 10", tree! {Sub(Add(Int(5), Int(15)), Int(10))})
}

#[test]
fn test_parse_parentheses() -> Result<(), Error> {
    parse_compare("5 + (15 - 10)", tree! {Add(Int(5), Sub(Int(15), Int(10)))})
}

#[test]
fn test_parse_multiply() -> Result<(), Error> {
    parse_compare("5 * 10", tree! {Mul(Int(5), Int(10))})
}

#[test]
fn test_parse_multiply_left_precedence() -> Result<(), Error> {
    parse_compare("5*10 + 15", tree! {Add(Mul(Int(5),Int(10)), Int(15))})
}

#[test]
fn test_parse_multiply_right_precedence() -> Result<(), Error> {
    parse_compare("5 + 10*15", tree! {Add(Int(5), Mul(Int(10),Int(15)))})
}

#[test]
fn test_parse_divide() -> Result<(), Error> {
    parse_compare("5 / 10", tree! {Div(Int(5), Int(10))})
}

#[test]
fn test_parse_mixed_multiply_divide() -> Result<(), Error> {
    parse_compare(
        "2 * 5 / 10 * 42",
        tree! {Mul(
        Div(Mul(Int(2), Int(5)),
            Int(10)),
           Int(42))},
    )
}

#[test]
fn test_literal_true() -> Result<(), Error> {
    parse_compare("true", tree! {Bool(true)})
}

#[test]
fn test_literal_false() -> Result<(), Error> {
    parse_compare("false", tree! {Bool(false)})
}

#[test]
fn test_positive_float() -> Result<(), Error> {
    parse_compare("1.5", tree! {Div(Int(3), Int(2))})
}

#[test]
fn test_negative_float() -> Result<(), Error> {
    parse_compare("-1.5", tree! {Div(Int(-3), Int(2))})
}

#[test]
fn test_positive_float_without_ipart() -> Result<(), Error> {
    parse_compare(".5", tree! {Div(Int(1), Int(2))})
}

#[test]
fn test_negative_float_without_ipart() -> Result<(), Error> {
    parse_compare("-.5", tree! {Div(Int(-1), Int(2))})
}

#[test]
fn test_positive_float_without_fpart() -> Result<(), Error> {
    parse_compare("1.", tree! {Int(1)})
}

#[test]
fn test_negative_float_without_fpart() -> Result<(), Error> {
    parse_compare("-1.", tree! {Int(-1)})
}

#[test]
fn test_float_with_leading_zero_in_fpart() -> Result<(), Error> {
    parse_compare("1.0625", tree! {Div(Int(17), Int(16))})
}

#[test]
fn test_standalone_dot_is_error() -> Result<(), Error> {
    assert!(Arena::try_build(|arena| parse_expr(".".chars(), arena)).is_err());
    Ok(())
}
