use algebra::{Bool, Int, Rational};
use std::fmt::Display;
use typed_dag::{Arena, RecursiveFamily, Visitable, VisitingRef, VisitorOf};
use typed_dag_derive::tree;

// Explicitly use a validate() function instead of adding these lines
// to the macro_rules! definition, as otherwise the use of the tree!
// macro in the generated code causes spurious underlining by
// rust-analyzer, as of 2023-04-22.
fn validate<'ext: 'view, 'view, Container, Target>(
    arena: &'view Arena<Container, Target>,
    expected: &str,
) where
    Target: RecursiveFamily<'ext>,
    VisitingRef<'view, Container>:
        VisitorOf<'ext, Target, Node<Target> = VisitingRef<'view, Container, Target>>,
    <Target as RecursiveFamily<'ext>>::Sibling<VisitingRef<'view, Container>>: Display,
{
    let formatted = format!("{}", arena.visit_root().expand());
    assert_eq!(formatted, expected);
}

macro_rules! test_cases {
    ($($name:ident: $expr:expr => $expected:expr,)*) => {
        $(
            #[test]
            fn $name() {
                validate(&tree![$expr], $expected)
            }
        )*
    };
}

test_cases! {
    int: Int::Literal(5) => "5",
    add: Int::Add(Int::Literal(5), Int::Literal(10)) => "5 + 10",
    sub: Int::Sub(Int::Literal(5), Int::Literal(10)) => "5 - 10",
    left_associative_add: Int::Add(
        Int::Add(Int::Literal(5), Int::Literal(10)),
        Int::Literal(15)
    ) => "5 + 10 + 15",
    right_associative_add: Int::Add(
        Int::Literal(5),
        Int::Add(Int::Literal(10), Int::Literal(15))
    ) => "5 + (10 + 15)",

    mul: Int::Mul(Int::Literal(5), Int::Literal(10)) => "5*10",
    div: Rational::Ratio(Int::Literal(5), Int::Literal(10)) => "5/10",

    left_associative_mul: Int::Mul(
        Int::Mul(Int::Literal(5), Int::Literal(10)),
        Int::Literal(15)
    ) => "5*10*15",

    right_associative_mul: Int::Mul(
        Int::Literal(5),
        Int::Mul(Int::Literal(10), Int::Literal(15))
    ) => "5*(10*15)",

    mul_lhs_add: Int::Mul(
        Int::Add(Int::Literal(5), Int::Literal(10)),
        Int::Literal(15)
    ) => "(5 + 10)*15",

    mul_rhs_add: Int::Mul(
        Int::Literal(5),
        Int::Add(Int::Literal(10), Int::Literal(15))
    ) => "5*(10 + 15)",

    add_lhs_mul: Int::Add(
        Int::Mul(Int::Literal(5), Int::Literal(10)),
        Int::Literal(15)
    ) => "5*10 + 15",

    add_rhs_mul: Int::Add(
        Int::Literal(5),
        Int::Mul(Int::Literal(10), Int::Literal(15))
    ) => "5 + 10*15",

    format_true: Bool::Literal(true) => "true",

    format_false: Bool::Literal(false) => "false",

    boolean_and: Bool::And(Bool::Literal(false), Bool::Literal(true)) => "false && true",

    boolean_or: Bool::Or(Bool::Literal(true), Bool::Literal(false)) => "true || false",

    floordiv: Int::FloorDiv(Int::Literal(5), Int::Literal(10)) => "5//10",

    floordiv_of_sum: Int::FloorDiv(
        Int::Add(Int::Literal(1), Int::Literal(2)),
        Int::Add(Int::Literal(3), Int::Literal(4))
    ) => "(1 + 2)//(3 + 4)",

    floormod: Int::FloorMod(Int::Literal(5), Int::Literal(10)) => "5%10",

    floormod_of_sum: Int::FloorMod(
        Int::Add(Int::Literal(1), Int::Literal(2)),
        Int::Add(Int::Literal(3), Int::Literal(4))
    ) => "(1 + 2)%(3 + 4)",
}
