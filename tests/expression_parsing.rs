use algebra::{parse_expr, Bool, Error, Expr, Int, Rational};
use typed_dag::{Arena, Visitable};
use typed_dag_derive::tree;

macro_rules! test_cases {
    ($($name:ident: $str:expr => $outer_type:ident :: $outer:ident($($inner:expr),*),)*) => {
        $(
            #[test]
            fn $name() -> Result<(), Error> {
                let parsed = Arena::try_build(|arena| parse_expr($str.chars(), arena))?;
                let expected = tree!{Expr::$outer_type($outer_type::$outer($($inner),*)) };
                assert_eq!(parsed.visit_root().expand(), expected.visit_root().expand());
                Ok(())
            }
        )*
    };
}

test_cases! {
    one_digit_integer: "5" => Int::Literal(5),
    two_digit_integer: "42" => Int::Literal(42),
    negative_integer: "-12" => Int::Literal(-12),
    literal_true: "true" => Bool::Literal(true),
    literal_false: "false" => Bool::Literal(false),

    positive_float: "1.5" => Rational::Ratio(Int::Literal(3), Int::Literal(2)),
    negative_float: "-1.5" => Rational::Ratio(Int::Literal(-3), Int::Literal(2)),
    positive_float_without_ipart: ".5" => Rational::Ratio(Int::Literal(1), Int::Literal(2)),
    negative_float_without_ipart: "-.5" => Rational::Ratio(Int::Literal(-1), Int::Literal(2)),
    positive_float_without_fpart: "1." => Int::Literal(1),
    negative_float_without_fpart: "-1." => Int::Literal(-1),
    float_with_leading_zero_in_fpart: "1.0625" => Rational::Ratio(Int::Literal(17), Int::Literal(16)),

    addition: "5+10" => Int::Add(Int::Literal(5), Int::Literal(10)),
    addition_with_spaces: "5 + 10" => Int::Add(Int::Literal(5), Int::Literal(10)),
    subtraction: "5 - 10" => Int::Sub(Int::Literal(5), Int::Literal(10)),
    multiply: "5 * 10" => Int::Mul(Int::Literal(5), Int::Literal(10)),
    divide: "5 / 10" => Rational::Ratio(Int::Literal(5), Int::Literal(10)),

    boolean_and: "true && false" => Bool::And(Bool::Literal(true), Bool::Literal(false)),
    boolean_or: "false || true" => Bool::Or(Bool::Literal(false), Bool::Literal(true)),

    multiple_addition: "5 + 10 + 15" => Int::Add(Int::Add(Int::Literal(5),Int::Literal(10)),
                                                 Int::Literal(15)),
    mixed_addition_subtraction: "5 + 15 - 10" => Int::Sub(Int::Add(Int::Literal(5), Int::Literal(15)),
                                                          Int::Literal(10)),
    parentheses: "5 + (15 - 10)" => Int::Add(Int::Literal(5),
                                             Int::Sub(Int::Literal(15), Int::Literal(10))),

    multiply_left_precedence: "5*10 + 15" => Int::Add(Int::Mul(Int::Literal(5),Int::Literal(10)), Int::Literal(15)),
    multiply_right_precedence: "5 + 10*15" => Int::Add(Int::Literal(5), Int::Mul(Int::Literal(10),Int::Literal(15))),

    mixed_multiply_divide: "2 * 5 / 10 * 42" => Rational::Mul(
        Rational::Ratio(Int::Mul(Int::Literal(2), Int::Literal(5)),
                        Int::Literal(10)),
        Rational::Int(Int::Literal(42))),
}

#[test]
fn test_standalone_dot_is_error() -> Result<(), Error> {
    assert!(Arena::try_build(|arena| parse_expr(".".chars(), arena)).is_err());
    Ok(())
}
