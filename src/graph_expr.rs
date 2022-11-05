#![allow(unused_imports)]
#![allow(dead_code)]

use crate::graph::{GraphRef, LiveGraphRef, NodeType, NodeTypeSelector, Subgraph};
use crate::Error;

use graph_derive::recursive_graph;

#[recursive_graph]
mod temp {
    use crate::graph::{GraphRef, LiveGraphRef, NodeType, NodeTypeSelector, Subgraph};

    // First one is special, defines the name of all the others.
    // Maybe replace in the future, special handling for
    // variants that reference just a single-reference type?
    //
    // Better idea: The macro should generate up to three structs
    // for each item.
    //
    // (1): A {name} that is the user-facing object.  It
    // represents a full expression, including the Vec<Item>
    // storage.  The storage type is either itself, or another
    // type that can hold all of the reachable types from itself.
    // If no such storage type exists, should raise compile-time
    // error.
    //
    // (2): A storage::{name} that is stored in the Vec<Item> node.
    // This is the in-memory representation, with all recursive
    // references replaced by GraphRef..
    //
    // (3): A live::{name} that is used when iterating.  This has
    // the same structure has the storage::{name}, but contains
    // LiveGraphRef instead of GraphRef.
    //
    // (4): A selector::{name} that is used to store all possible
    // storage::* types inside a single vector.  It contains all
    // recursive types that are directly or indirectly reachable from
    // {name}.

    // TODO
    // enum Basic<T> {
    //     Literal(T),
    //     Add(Basic<T>, Basic<T>),
    //     Sub(Basic<T>, Basic<T>),
    // }

    // enum IndirectTest {
    //     Int(i64),
    //     Recursive(IndirectTest),
    // }

    enum IntExpr {
        Int(i64),
        //Indirect(IndirectTest),
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
enum LiveIntExpr<'a, Selector: NodeTypeSelector> {
    Int(i64),
    Add(
        LiveGraphRef<'a, Selector, IntExpr>,
        LiveGraphRef<'a, Selector, IntExpr>,
    ),
    Sub(
        LiveGraphRef<'a, Selector, IntExpr>,
        LiveGraphRef<'a, Selector, IntExpr>,
    ),
}

#[derive(Debug)]
enum LiveFloatExpr<'a, Selector: NodeTypeSelector> {
    Float(f64),
    Add(
        LiveGraphRef<'a, Selector, FloatExpr>,
        LiveGraphRef<'a, Selector, FloatExpr>,
    ),
    Sub(
        LiveGraphRef<'a, Selector, FloatExpr>,
        LiveGraphRef<'a, Selector, FloatExpr>,
    ),
}

#[derive(Debug)]
enum LiveBoolExpr<'a, Selector: NodeTypeSelector> {
    Bool(bool),
    IntEqual(
        LiveGraphRef<'a, Selector, IntExpr>,
        LiveGraphRef<'a, Selector, IntExpr>,
    ),
    FloatEqual(
        LiveGraphRef<'a, Selector, FloatExpr>,
        LiveGraphRef<'a, Selector, FloatExpr>,
    ),
    And(
        LiveGraphRef<'a, Selector, BoolExpr>,
        LiveGraphRef<'a, Selector, BoolExpr>,
    ),
    Or(
        LiveGraphRef<'a, Selector, BoolExpr>,
        LiveGraphRef<'a, Selector, BoolExpr>,
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

impl NodeTypeSelector for Expr {
    type LiveType<'a> = LiveExpr<'a>;

    fn type_name(&self) -> &'static str {
        match self {
            Expr::IntExpr(_) => "IntExpr",
            Expr::FloatExpr(_) => "FloatExpr",
            Expr::BoolExpr(_) => "BoolExpr",
        }
    }

    fn to_live_type<'a, 'b: 'a>(&self, subgraph: Subgraph<'b, Self>) -> Self::LiveType<'a> {
        match self {
            Expr::IntExpr(e) => LiveExpr::IntExpr(e.to_live_type(subgraph)),
            Expr::FloatExpr(e) => LiveExpr::FloatExpr(e.to_live_type(subgraph)),
            Expr::BoolExpr(e) => LiveExpr::BoolExpr(e.to_live_type(subgraph)),
        }
    }
}

impl NodeType<Expr> for IntExpr {
    type LiveType<'a> = LiveIntExpr<'a, Expr>;
    const NAME: &'static str = "IntExpr";

    fn from_base(base: &Expr) -> Option<&Self> {
        match base {
            Expr::IntExpr(e) => Some(e),
            _ => None,
        }
    }

    fn to_live_type<'a, 'b: 'a>(&self, subgraph: Subgraph<'b, Expr>) -> Self::LiveType<'a> {
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

impl NodeType<Expr> for FloatExpr {
    type LiveType<'a> = LiveFloatExpr<'a, Expr>;
    const NAME: &'static str = "FloatExpr";

    fn from_base(base: &Expr) -> Option<&Self> {
        match base {
            Expr::FloatExpr(e) => Some(e),
            _ => None,
        }
    }

    fn to_live_type<'a, 'b: 'a>(&self, subgraph: Subgraph<'b, Expr>) -> Self::LiveType<'a> {
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

impl NodeType<Expr> for BoolExpr {
    type LiveType<'a> = LiveBoolExpr<'a, Expr>;
    const NAME: &'static str = "BoolExpr";

    fn from_base(base: &Expr) -> Option<&Self> {
        match base {
            Expr::BoolExpr(e) => Some(e),
            _ => None,
        }
    }

    fn to_live_type<'a, 'b: 'a>(&self, subgraph: Subgraph<'b, Expr>) -> Self::LiveType<'a> {
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

        let root: LiveExpr = expr.root();

        match root {
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
