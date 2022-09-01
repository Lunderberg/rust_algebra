#![allow(unused_imports)]
#![allow(dead_code)]

use crate::graph::{GraphRef, LiveGraphRef, NodeType, Subgraph};
use crate::Error;

mod temp {
    use crate::graph::{GraphRef, LiveGraphRef, NodeType, Subgraph};
    use graph_derive::make_graph;

    make_graph! {
        // First one is special, defines the name of all the others.
        // Maybe replace in the future, special handling for
        // variants that reference just a single-reference type?
        //
        // Better idea,
        enum Expr {
            IntExpr(IntExpr),
            FloatExpr(FloatExpr),
            BoolExpr(BoolExpr),
        }

        enum IntExpr {
            Int(i64),
            Add(IntExpr, IntExpr),
            Sub(IntExpr, IntExpr),
        }

        enum FloatExpr {
            Float(f64),
            Add(FloatExpr, FloatExpr),
            Sub(FloatExpr, FloatExpr),
        }

        enum BoolExpr {
            Bool(bool),
            IntEqual(IntExpr, IntExpr),
            FloatEqual(FloatExpr, FloatExpr),
            And(BoolExpr, BoolExpr),
            Or(BoolExpr, BoolExpr),
        }
    }
}

#[derive(Debug)]
enum Expr {
    IntExpr(IntExpr),
    FloatExpr(FloatExpr),
    BoolExpr(BoolExpr),
}

#[derive(Debug)]
enum IntExpr {
    Int(i64),
    Add(GraphRef<IntExpr>, GraphRef<IntExpr>),
    Sub(GraphRef<IntExpr>, GraphRef<IntExpr>),
}

#[derive(Debug)]
enum FloatExpr {
    Float(f64),
    Add(GraphRef<FloatExpr>, GraphRef<FloatExpr>),
    Sub(GraphRef<FloatExpr>, GraphRef<FloatExpr>),
}

#[derive(Debug)]
enum BoolExpr {
    Bool(bool),
    IntEqual(GraphRef<IntExpr>, GraphRef<IntExpr>),
    FloatEqual(GraphRef<FloatExpr>, GraphRef<FloatExpr>),
    And(GraphRef<BoolExpr>, GraphRef<BoolExpr>),
    Or(GraphRef<BoolExpr>, GraphRef<BoolExpr>),
}

#[derive(Debug)]
enum LiveExpr<'a> {
    IntExpr(LiveIntExpr<'a, Expr>),
    FloatExpr(LiveFloatExpr<'a, Expr>),
    BoolExpr(LiveBoolExpr<'a, Expr>),
}

#[derive(Debug)]
enum LiveIntExpr<'a, NodeBase> {
    Int(i64),
    Add(
        LiveGraphRef<'a, NodeBase, IntExpr>,
        LiveGraphRef<'a, NodeBase, IntExpr>,
    ),
    Sub(
        LiveGraphRef<'a, NodeBase, IntExpr>,
        LiveGraphRef<'a, NodeBase, IntExpr>,
    ),
}

#[derive(Debug)]
enum LiveFloatExpr<'a, NodeBase> {
    Float(f64),
    Add(
        LiveGraphRef<'a, NodeBase, FloatExpr>,
        LiveGraphRef<'a, NodeBase, FloatExpr>,
    ),
    Sub(
        LiveGraphRef<'a, NodeBase, FloatExpr>,
        LiveGraphRef<'a, NodeBase, FloatExpr>,
    ),
}

#[derive(Debug)]
enum LiveBoolExpr<'a, NodeBase> {
    Bool(bool),
    IntEqual(
        LiveGraphRef<'a, NodeBase, IntExpr>,
        LiveGraphRef<'a, NodeBase, IntExpr>,
    ),
    FloatEqual(
        LiveGraphRef<'a, NodeBase, FloatExpr>,
        LiveGraphRef<'a, NodeBase, FloatExpr>,
    ),
    And(
        LiveGraphRef<'a, NodeBase, BoolExpr>,
        LiveGraphRef<'a, NodeBase, BoolExpr>,
    ),
    Or(
        LiveGraphRef<'a, NodeBase, BoolExpr>,
        LiveGraphRef<'a, NodeBase, BoolExpr>,
    ),
}

impl Expr {
    fn type_string(&self) -> &str {
        match self {
            Expr::IntExpr(_) => "IntExpr",
            Expr::FloatExpr(_) => "FloatExpr",
            Expr::BoolExpr(_) => "BoolExpr",
        }
    }
}

impl<'a, 'b> NodeType<'a, 'b, Expr> for Expr
where
    'a: 'b,
{
    type LiveType = LiveExpr<'b>;

    fn from_base(base: &Expr) -> Option<&Self> {
        Some(base)
    }

    fn to_live_type(&self, subgraph: Subgraph<'a, Expr>) -> Self::LiveType {
        match self {
            Expr::IntExpr(e) => LiveExpr::IntExpr(e.to_live_type(subgraph)),
            Expr::FloatExpr(e) => LiveExpr::FloatExpr(e.to_live_type(subgraph)),
            Expr::BoolExpr(e) => LiveExpr::BoolExpr(e.to_live_type(subgraph)),
        }
    }

    fn class_type() -> &'static str {
        "Expr"
    }
}

impl<'a, 'b> NodeType<'a, 'b, Expr> for IntExpr
where
    'a: 'b,
{
    type LiveType = LiveIntExpr<'b, Expr>;

    fn from_base(base: &Expr) -> Option<&Self> {
        match base {
            Expr::IntExpr(e) => Some(e),
            _ => None,
        }
    }

    fn to_live_type(&self, subgraph: Subgraph<'a, Expr>) -> Self::LiveType {
        match self {
            IntExpr::Int(i) => LiveIntExpr::Int(*i),
            IntExpr::Add(a, b) => LiveIntExpr::Add(
                LiveGraphRef::new(*a, subgraph.clone()),
                LiveGraphRef::new(*b, subgraph),
            ),
            IntExpr::Sub(a, b) => LiveIntExpr::Sub(
                LiveGraphRef::new(*a, subgraph.clone()),
                LiveGraphRef::new(*b, subgraph),
            ),
        }
    }

    fn class_type() -> &'static str {
        "IntExpr"
    }
}

impl<'a, 'b> NodeType<'a, 'b, Expr> for FloatExpr
where
    'a: 'b,
{
    type LiveType = LiveFloatExpr<'b, Expr>;

    fn from_base(base: &Expr) -> Option<&Self> {
        match base {
            Expr::FloatExpr(e) => Some(e),
            _ => None,
        }
    }

    fn to_live_type(&self, subgraph: Subgraph<'a, Expr>) -> Self::LiveType {
        match self {
            FloatExpr::Float(f) => LiveFloatExpr::Float(*f),
            FloatExpr::Add(a, b) => LiveFloatExpr::Add(
                LiveGraphRef::new(*a, subgraph.clone()),
                LiveGraphRef::new(*b, subgraph),
            ),
            FloatExpr::Sub(a, b) => LiveFloatExpr::Sub(
                LiveGraphRef::new(*a, subgraph.clone()),
                LiveGraphRef::new(*b, subgraph),
            ),
        }
    }

    fn class_type() -> &'static str {
        "FloatExpr"
    }
}

impl<'a, 'b> NodeType<'a, 'b, Expr> for BoolExpr
where
    'a: 'b,
{
    type LiveType = LiveBoolExpr<'b, Expr>;

    fn from_base(base: &Expr) -> Option<&Self> {
        match base {
            Expr::BoolExpr(e) => Some(e),
            _ => None,
        }
    }

    fn to_live_type(&self, subgraph: Subgraph<'a, Expr>) -> Self::LiveType {
        match self {
            BoolExpr::Bool(b) => LiveBoolExpr::Bool(*b),
            BoolExpr::IntEqual(a, b) => LiveBoolExpr::IntEqual(
                LiveGraphRef::new(*a, subgraph.clone()),
                LiveGraphRef::new(*b, subgraph),
            ),
            BoolExpr::FloatEqual(a, b) => LiveBoolExpr::FloatEqual(
                LiveGraphRef::new(*a, subgraph.clone()),
                LiveGraphRef::new(*b, subgraph),
            ),
            BoolExpr::And(a, b) => LiveBoolExpr::And(
                LiveGraphRef::new(*a, subgraph.clone()),
                LiveGraphRef::new(*b, subgraph),
            ),
            BoolExpr::Or(a, b) => LiveBoolExpr::Or(
                LiveGraphRef::new(*a, subgraph.clone()),
                LiveGraphRef::new(*b, subgraph),
            ),
        }
    }

    fn class_type() -> &'static str {
        "BoolExpr"
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Error;
    use crate::Graph;

    #[test]
    fn test_basic() -> Result<(), Error> {
        // let expr: Graph<Expr> = Graph::new(Expr::Sub(
        //     Expr::Add(Expr::Int(5), Expr::Int(15)),
        //     Expr::Int(10),
        // ));
        let expr = Graph::new(vec![
            Expr::IntExpr(IntExpr::Int(5)),
            Expr::IntExpr(IntExpr::Int(15)),
            Expr::IntExpr(IntExpr::Add(2.into(), 1.into())),
            Expr::IntExpr(IntExpr::Int(10)),
            Expr::IntExpr(IntExpr::Sub(2.into(), 1.into())),
        ])?;

        let root: LiveGraphRef<'_, Expr, Expr> = expr.root();
        let borrowed: LiveExpr = root.borrow()?;

        match borrowed {
            LiveExpr::IntExpr(i) => {
                println!("Found int expression, {i:?}");
                match i {
                    LiveIntExpr::Int(a) => {
                        println!("IntLiteral {a:?}")
                    }
                    LiveIntExpr::Add(a, b) => {
                        println!("Addition of {a:?} and {b:?}")
                    }
                    LiveIntExpr::Sub(a, b) => {
                        println!(
                            "Subtraction of {a:?} and {b:?}, which are {:?} and {:?}",
                            a.borrow()?,
                            b.borrow()?
                        )
                    }
                }
            }
            LiveExpr::FloatExpr(_f) => {
                println!("Found float expression");
            }
            LiveExpr::BoolExpr(_b) => {
                println!("Found bool expression");
            }
        }

        Ok(())
    }
}
