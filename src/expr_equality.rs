use std::cmp::PartialEq;

use crate::expr;
use typed_dag::Visitable;

impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Expr<V> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Int(l0) => matches!(other, Self::Int(r0) if l0.borrow() == r0.borrow()),
            Self::Bool(l0) => matches!(other,  Self::Bool(r0) if l0.borrow() == r0.borrow()),
            Self::Rational(l0) => matches!(other, Self::Rational(r0)if l0.borrow() == r0.borrow()),
        }
    }
}
impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Int<V> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Literal(l0) => matches!(other, Self::Literal(r0) if l0 == r0),
            Self::Add(l0, l1) => {
                matches!(other, Self::Add(r0,r1) if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow())
            }
            Self::Sub(l0, l1) => {
                matches!(other, Self::Sub(r0,r1) if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow())
            }
            Self::Mul(l0, l1) => {
                matches!(other, Self::Mul(r0,r1) if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow())
            }
            Self::FloorDiv(l0, l1) => {
                matches!(other, Self::FloorDiv(r0,r1) if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow())
            }
            Self::FloorMod(l0, l1) => {
                matches!(other, Self::FloorMod(r0,r1) if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow())
            }
            Self::Negative(l0) => matches!(other, Self::Negative(r0) if l0.borrow() == r0.borrow()),
        }
    }
}
impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Bool<V> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Literal(l0) => matches!(other, Self::Literal(r0) if l0 == r0),
            Self::IntEqual(l0, l1) => matches!(other, Self::IntEqual(r0, r1) if
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()),

            Self::RationalEqual(l0, l1) => matches!(other, Self::RationalEqual(r0, r1) if
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()),

            Self::Not(l0) => matches!(other, Self::Not(r0) if l0.borrow() == r0.borrow()),
            Self::And(l0, l1) => {
                matches!(other, Self::And(r0, r1) if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow())
            }
            Self::Or(l0, l1) => {
                matches!(other, Self::Or(r0, r1) if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow())
            }
        }
    }
}
impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Rational<V> {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::Int(l0) => {
                matches!(other, Self::Int(r0)       if l0.borrow() == r0.borrow                               ())
            }
            Self::Ratio(l0, l1) => {
                matches!(other, Self::Ratio(r0, r1) if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow ())
            }
            Self::Negative(l0) => {
                matches!(other, Self::Negative(r0)  if l0.borrow() == r0.borrow                               ())
            }
            Self::Add(l0, l1) => {
                matches!(other, Self::Add(r0, r1)   if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow ())
            }
            Self::Sub(l0, l1) => {
                matches!(other, Self::Sub(r0, r1)   if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow ())
            }
            Self::Mul(l0, l1) => {
                matches!(other, Self::Mul(r0, r1)   if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow ())
            }
            Self::Div(l0, l1) => {
                matches!(other, Self::Div(r0, r1)   if l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow ())
            }
        }
    }
}
