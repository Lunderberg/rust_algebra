use std::cmp::PartialEq;

use crate::expr;
use typed_dag::Visitable;

impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Expr<V> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Int(l0) => matches!(other, Self::Int(r0) if l0.expand() == r0.expand()),
            Self::Bool(l0) => matches!(other,  Self::Bool(r0) if l0.expand() == r0.expand()),
            Self::Rational(l0) => matches!(other, Self::Rational(r0)if l0.expand() == r0.expand()),
        }
    }
}
impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Int<V> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Literal(l0) => matches!(other, Self::Literal(r0) if l0 == r0),
            Self::Add(l0, l1) => {
                matches!(other, Self::Add(r0,r1) if l0.expand() == r0.expand() && l1.expand() == r1.expand())
            }
            Self::Sub(l0, l1) => {
                matches!(other, Self::Sub(r0,r1) if l0.expand() == r0.expand() && l1.expand() == r1.expand())
            }
            Self::Mul(l0, l1) => {
                matches!(other, Self::Mul(r0,r1) if l0.expand() == r0.expand() && l1.expand() == r1.expand())
            }
            Self::FloorDiv(l0, l1) => {
                matches!(other, Self::FloorDiv(r0,r1) if l0.expand() == r0.expand() && l1.expand() == r1.expand())
            }
            Self::FloorMod(l0, l1) => {
                matches!(other, Self::FloorMod(r0,r1) if l0.expand() == r0.expand() && l1.expand() == r1.expand())
            }
            Self::Negative(l0) => matches!(other, Self::Negative(r0) if l0.expand() == r0.expand()),
        }
    }
}
impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Bool<V> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Literal(l0) => matches!(other, Self::Literal(r0) if l0 == r0),
            Self::IntEqual(l0, l1) => matches!(other, Self::IntEqual(r0, r1) if
                l0.expand() == r0.expand() && l1.expand() == r1.expand()),

            Self::RationalEqual(l0, l1) => matches!(other, Self::RationalEqual(r0, r1) if
                l0.expand() == r0.expand() && l1.expand() == r1.expand()),

            Self::Not(l0) => matches!(other, Self::Not(r0) if l0.expand() == r0.expand()),
            Self::And(l0, l1) => {
                matches!(other, Self::And(r0, r1) if l0.expand() == r0.expand() && l1.expand() == r1.expand())
            }
            Self::Or(l0, l1) => {
                matches!(other, Self::Or(r0, r1) if l0.expand() == r0.expand() && l1.expand() == r1.expand())
            }
        }
    }
}
impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Rational<V> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Int(l0) => {
                matches!(other, Self::Int(r0)       if l0.expand() == r0.expand                               ())
            }
            Self::Ratio(l0, l1) => {
                matches!(other, Self::Ratio(r0, r1) if l0.expand() == r0.expand() && l1.expand() == r1.expand ())
            }
            Self::Negative(l0) => {
                matches!(other, Self::Negative(r0)  if l0.expand() == r0.expand                               ())
            }
            Self::Add(l0, l1) => {
                matches!(other, Self::Add(r0, r1)   if l0.expand() == r0.expand() && l1.expand() == r1.expand ())
            }
            Self::Sub(l0, l1) => {
                matches!(other, Self::Sub(r0, r1)   if l0.expand() == r0.expand() && l1.expand() == r1.expand ())
            }
            Self::Mul(l0, l1) => {
                matches!(other, Self::Mul(r0, r1)   if l0.expand() == r0.expand() && l1.expand() == r1.expand ())
            }
            Self::Div(l0, l1) => {
                matches!(other, Self::Div(r0, r1)   if l0.expand() == r0.expand() && l1.expand() == r1.expand ())
            }
        }
    }
}
