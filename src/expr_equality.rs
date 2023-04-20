use std::cmp::PartialEq;

use crate::expr;
use typed_dag::Visitable;

impl<'view, V: expr::visitor::Expr<'view>> PartialEq for expr::Expr<V> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(lhs), Self::Int(rhs)) => lhs == rhs,
            (Self::Bool(lhs), Self::Bool(rhs)) => lhs == rhs,
            (Self::Add(lhs_a, lhs_b), Self::Add(rhs_a, rhs_b)) => {
                lhs_a.borrow() == rhs_a.borrow() && lhs_b.borrow() == rhs_b.borrow()
            }
            (Self::Sub(lhs_a, lhs_b), Self::Sub(rhs_a, rhs_b)) => {
                lhs_a.borrow() == rhs_a.borrow() && lhs_b.borrow() == rhs_b.borrow()
            }
            (Self::Mul(lhs_a, lhs_b), Self::Mul(rhs_a, rhs_b)) => {
                lhs_a.borrow() == rhs_a.borrow() && lhs_b.borrow() == rhs_b.borrow()
            }
            (Self::Div(lhs_a, lhs_b), Self::Div(rhs_a, rhs_b)) => {
                lhs_a.borrow() == rhs_a.borrow() && lhs_b.borrow() == rhs_b.borrow()
            }
            (Self::Equal(lhs_a, lhs_b), Self::Equal(rhs_a, rhs_b)) => {
                lhs_a.borrow() == rhs_a.borrow() && lhs_b.borrow() == rhs_b.borrow()
            }
            (Self::And(lhs_a, lhs_b), Self::And(rhs_a, rhs_b)) => {
                lhs_a.borrow() == rhs_a.borrow() && lhs_b.borrow() == rhs_b.borrow()
            }
            (Self::Or(lhs_a, lhs_b), Self::Or(rhs_a, rhs_b)) => {
                lhs_a.borrow() == rhs_a.borrow() && lhs_b.borrow() == rhs_b.borrow()
            }
            _ => false,
        }
    }
}
