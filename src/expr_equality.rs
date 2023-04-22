use std::cmp::PartialEq;

use crate::expr;
use typed_dag::Visitable;

impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Expr<V> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0.borrow() == r0.borrow(),
            (Self::Bool(l0), Self::Bool(r0)) => l0.borrow() == r0.borrow(),
            (Self::Rational(l0), Self::Rational(r0)) => l0.borrow() == r0.borrow(),
            _ => false,
        }
    }
}
impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Int<V> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Literal(l0), Self::Literal(r0)) => l0 == r0,
            (Self::Add(l0, l1), Self::Add(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::Sub(l0, l1), Self::Sub(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::Mul(l0, l1), Self::Mul(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::Negative(l0), Self::Negative(r0)) => l0.borrow() == r0.borrow(),
            _ => false,
        }
    }
}
impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Bool<V> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Literal(l0), Self::Literal(r0)) => l0 == r0,
            (Self::IntEqual(l0, l1), Self::IntEqual(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::RationalEqual(l0, l1), Self::RationalEqual(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::Not(l0), Self::Not(r0)) => l0.borrow() == r0.borrow(),
            (Self::And(l0, l1), Self::And(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::Or(l0, l1), Self::Or(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            _ => false,
        }
    }
}
impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Rational<V> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0.borrow() == r0.borrow(),
            (Self::Ratio(l0, l1), Self::Ratio(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::Negative(l0), Self::Negative(r0)) => l0.borrow() == r0.borrow(),
            (Self::Add(l0, l1), Self::Add(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::Sub(l0, l1), Self::Sub(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::Mul(l0, l1), Self::Mul(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            (Self::Div(l0, l1), Self::Div(r0, r1)) => {
                l0.borrow() == r0.borrow() && l1.borrow() == r1.borrow()
            }
            _ => false,
        }
    }
}
