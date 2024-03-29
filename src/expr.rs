use typed_dag::NodeRefType;
use typed_dag_derive::typed_dag;

#[typed_dag]
mod expr {
    pub enum Expr {
        Int(Int),
        Bool(Bool),
        Rational(Rational),
    }

    pub enum Int {
        Literal(i64),
        Add(Int, Int),
        Sub(Int, Int),
        Mul(Int, Int),
        //Floor(RationalExpr),
        FloorDiv(Int, Int),
        FloorMod(Int, Int),
        Negative(Int),
    }
    pub enum Bool {
        Literal(bool),
        IntEqual(Int, Int),
        RationalEqual(Rational, Rational),

        Not(Bool),
        And(Bool, Bool),
        Or(Bool, Bool),
    }
    pub enum Rational {
        Int(Int),
        Ratio(Int, Int),
        Negative(Rational),
        Add(Rational, Rational),
        Sub(Rational, Rational),
        Mul(Rational, Rational),
        Div(Rational, Rational),
    }
}
pub use expr::*;

#[derive(PartialOrd, Ord, PartialEq, Eq)]
pub(crate) enum OperatorPrecedence {
    Expr,
    UnaryNeg,
    AddSub,
    MulDiv,
    Comparison,
    Boolean,
}

pub(crate) trait BinaryOperator {
    fn precedence(&self) -> Option<OperatorPrecedence>;
    fn display_str(&self) -> Option<&str>;
}

impl<R: NodeRefType<'static>> BinaryOperator for Int<R> {
    fn precedence(&self) -> Option<OperatorPrecedence> {
        match self {
            Int::Literal(_) => None,
            Int::Add(_, _) => Some(OperatorPrecedence::AddSub),
            Int::Sub(_, _) => Some(OperatorPrecedence::AddSub),
            Int::Mul(_, _) => Some(OperatorPrecedence::MulDiv),
            Int::FloorMod(_, _) => Some(OperatorPrecedence::MulDiv),
            Int::FloorDiv(_, _) => Some(OperatorPrecedence::MulDiv),
            Int::Negative(_) => Some(OperatorPrecedence::UnaryNeg),
        }
    }

    fn display_str(&self) -> Option<&str> {
        match self {
            Int::Literal(_) => None,
            Int::Negative(_) => Some("-"),
            Int::Add(_, _) => Some(" + "),
            Int::Sub(_, _) => Some(" - "),
            Int::Mul(_, _) => Some("*"),
            Int::FloorDiv(_, _) => Some("//"),
            Int::FloorMod(_, _) => Some("%"),
        }
    }
}

impl<R: NodeRefType<'static>> BinaryOperator for Bool<R> {
    fn precedence(&self) -> Option<OperatorPrecedence> {
        match self {
            Bool::Literal(_) => None,
            Bool::And(_, _) => Some(OperatorPrecedence::Boolean),
            Bool::Or(_, _) => Some(OperatorPrecedence::Boolean),
            Bool::Not(_) => Some(OperatorPrecedence::Boolean),
            Bool::IntEqual(_, _) => Some(OperatorPrecedence::Comparison),
            Bool::RationalEqual(_, _) => Some(OperatorPrecedence::Comparison),
        }
    }

    fn display_str(&self) -> Option<&str> {
        match self {
            Bool::Literal(_) => None,
            Bool::And(_, _) => Some(" && "),
            Bool::Or(_, _) => Some(" || "),
            Bool::Not(_) => Some("!"),
            Bool::IntEqual(_, _) => Some("=="),
            Bool::RationalEqual(_, _) => Some("=="),
        }
    }
}

impl<R: NodeRefType<'static>> BinaryOperator for Rational<R> {
    fn precedence(&self) -> Option<OperatorPrecedence> {
        match self {
            Rational::Int(_) => None,
            Rational::Negative(_) => Some(OperatorPrecedence::UnaryNeg),
            Rational::Add(_, _) => Some(OperatorPrecedence::AddSub),
            Rational::Sub(_, _) => Some(OperatorPrecedence::AddSub),
            Rational::Mul(_, _) => Some(OperatorPrecedence::MulDiv),
            Rational::Div(_, _) => Some(OperatorPrecedence::MulDiv),
            Rational::Ratio(_, _) => Some(OperatorPrecedence::MulDiv),
        }
    }

    fn display_str(&self) -> Option<&str> {
        match self {
            Rational::Int(_) => None,
            Rational::Ratio(_, _) => Some("/"),
            Rational::Negative(_) => Some("-"),
            Rational::Add(_, _) => Some(" + "),
            Rational::Sub(_, _) => Some(" - "),
            Rational::Mul(_, _) => Some("*"),
            Rational::Div(_, _) => Some("/"),
        }
    }
}
