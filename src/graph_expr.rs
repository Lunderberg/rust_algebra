#![allow(unused_imports)]
#![allow(dead_code)]

use graph_derive::make_graph;

use crate::graph::{GraphRef, LiveGraphRef, Subgraph};
use crate::{Error, Result};

make_graph! {
    // First enum must have no items, becomes the template for the
    // storage enum.
    #[derive(Debug)]
    enum Expr {}

    #[derive(Debug)]
    enum IntExpr {
        Int(i64),
        Add(IntExpr, IntExpr),
        Sub(IntExpr, IntExpr),
    }

    #[derive(Debug)]
    enum FloatExpr {
        Float(f64),
        Add(FloatExpr, FloatExpr),
        Sub(FloatExpr, FloatExpr),
    }

    #[derive(Debug)]
    enum BoolExpr {
        Bool(bool),
        IntEqual(IntExpr, IntExpr),
        FloatEqual(FloatExpr, FloatExpr),
        And(BoolExpr, BoolExpr),
        Or(BoolExpr, BoolExpr),
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

impl Expr {
    fn to_live<'a, 'b>(&self, subgraph: Subgraph<'a, Expr>) -> LiveExpr<'b>
    where
        'a: 'b,
    {
        match self {
            Expr::IntExpr(e) => LiveExpr::IntExpr(e.to_live(subgraph)),
            Expr::FloatExpr(e) => LiveExpr::FloatExpr(e.to_live(subgraph)),
            Expr::BoolExpr(e) => LiveExpr::BoolExpr(e.to_live(subgraph)),
        }
    }
}

impl IntExpr {
    fn to_live<'a, 'b, Base>(&self, subgraph: Subgraph<'a, Base>) -> LiveIntExpr<'b, Base>
    where
        'a: 'b,
    {
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
}

impl FloatExpr {
    fn to_live<'a, 'b, Base>(&self, subgraph: Subgraph<'a, Base>) -> LiveFloatExpr<'b, Base>
    where
        'a: 'b,
    {
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
}

impl BoolExpr {
    fn to_live<'a, 'b, Base>(&self, subgraph: Subgraph<'a, Base>) -> LiveBoolExpr<'b, Base>
    where
        'a: 'b,
    {
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
}

impl<'a> From<Subgraph<'a, Expr>> for LiveExpr<'a> {
    fn from(subgraph: Subgraph<'a, Expr>) -> Self {
        let node: &Expr = subgraph.node();
        node.to_live(subgraph.clone())
    }
}

impl<'a> From<Subgraph<'a, IntExpr>> for LiveIntExpr<'a, IntExpr> {
    fn from(subgraph: Subgraph<'a, IntExpr>) -> Self {
        let node: &IntExpr = subgraph.node();
        node.to_live(subgraph.clone())
    }
}

impl<'a> From<Subgraph<'a, FloatExpr>> for LiveFloatExpr<'a, FloatExpr> {
    fn from(subgraph: Subgraph<'a, FloatExpr>) -> Self {
        let node: &FloatExpr = subgraph.node();
        node.to_live(subgraph.clone())
    }
}

// This implementation shouldn't be made, because a BoolExpr can
// depend on IntExpr/FloatExpr nodes.  Therefore, whatever the
// storagetype must be able to represent those as well.  Proc macro
// will need to track the type dependency graph in order to determine
// the allowed backing type(s) for each node type.
//
// impl<'a> From<Subgraph<'a, BoolExpr>> for LiveBoolExpr<'a> {
//     fn from(subgraph: Subgraph<'a, BoolExpr>) -> Self {
//         let node: &BoolExpr = subgraph.node();
//         node.to_live(subgraph)
//     }
// }

impl<'a> TryFrom<Subgraph<'a, Expr>> for LiveIntExpr<'a, Expr> {
    type Error = Error;

    fn try_from(subgraph: Subgraph<'a, Expr>) -> Result<Self> {
        match subgraph.node() {
            Expr::IntExpr(e) => Ok(e.to_live(subgraph.clone())),
            node => Err(Error::IncorrectType {
                expected: "IntExpr".to_string(),
                actual: node.type_string().to_string(),
            }),
        }
    }
}

impl<'a> TryFrom<Subgraph<'a, Expr>> for LiveFloatExpr<'a, Expr> {
    type Error = Error;

    fn try_from(subgraph: Subgraph<'a, Expr>) -> Result<Self> {
        match subgraph.node() {
            Expr::FloatExpr(e) => Ok(e.to_live(subgraph.clone())),
            node => Err(Error::IncorrectType {
                expected: "FloatExpr".to_string(),
                actual: node.type_string().to_string(),
            }),
        }
    }
}

impl<'a> TryFrom<Subgraph<'a, Expr>> for LiveBoolExpr<'a, Expr> {
    type Error = Error;

    fn try_from(subgraph: Subgraph<'a, Expr>) -> Result<Self> {
        match subgraph.node() {
            Expr::BoolExpr(e) => Ok(e.to_live(subgraph.clone())),
            node => Err(Error::IncorrectType {
                expected: "BoolExpr".to_string(),
                actual: node.type_string().to_string(),
            }),
        }
    }
}

// The borrow methods can't be replaced with Deref because we need to
// return a LiveGraphRef value, which doesn't already exist.  It also
// can't be done with a trait, because the return type would need a
// generic associated type in order to have the correct lifetime.
//
// May be able to have cleaner usage with std::ops::Try
// implementation.
//
// GAT tracking issue: https://github.com/rust-lang/rust/issues/44265
// Try-trait tracking issue: https://github.com/rust-lang/rust/issues/84277

impl<'a> LiveGraphRef<'a, Expr, Expr> {
    #[allow(dead_code)]
    fn borrow<'b>(&self) -> Result<LiveExpr<'b>>
    where
        'a: 'b,
    {
        Ok(self.get_subgraph()?.into())
    }
}

impl<'a> LiveGraphRef<'a, IntExpr, IntExpr> {
    #[allow(dead_code)]
    fn borrow<'b>(&self) -> Result<LiveIntExpr<'b, IntExpr>>
    where
        'a: 'b,
    {
        Ok(self.get_subgraph()?.into())
    }
}

impl<'a> LiveGraphRef<'a, FloatExpr, FloatExpr> {
    #[allow(dead_code)]
    fn borrow<'b>(&self) -> Result<LiveFloatExpr<'b, FloatExpr>>
    where
        'a: 'b,
    {
        Ok(self.get_subgraph()?.into())
    }
}

// impl<'a> LiveGraphRef<'a, BoolExpr, BoolExpr> {
//     #[allow(dead_code)]
//     fn borrow<'b>(&self) -> Result<LiveBoolExpr<'b, BoolExpr>>
//     where
//         'a: 'b,
//     {
//         Ok(self.get_subgraph()?.into())
//     }
// }

impl<'a> LiveGraphRef<'a, Expr, IntExpr> {
    #[allow(dead_code)]
    fn borrow<'b>(&self) -> Result<LiveIntExpr<'b, Expr>>
    where
        'a: 'b,
    {
        self.get_subgraph()?.try_into()
    }
}

impl<'a> LiveGraphRef<'a, Expr, FloatExpr> {
    #[allow(dead_code)]
    fn borrow<'b>(&self) -> Result<LiveFloatExpr<'b, Expr>>
    where
        'a: 'b,
    {
        self.get_subgraph()?.try_into()
    }
}

impl<'a> LiveGraphRef<'a, Expr, BoolExpr> {
    #[allow(dead_code)]
    fn borrow<'b>(&self) -> Result<LiveBoolExpr<'b, Expr>>
    where
        'a: 'b,
    {
        self.get_subgraph()?.try_into()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Graph;
    use crate::Result;

    #[test]
    fn test_basic() -> Result<()> {
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
