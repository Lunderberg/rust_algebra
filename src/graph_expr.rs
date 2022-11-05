#![allow(unused_imports)]
#![allow(dead_code)]

use crate::graph::{GraphRef, LiveGraphRef, NodeType, NodeTypeSelector, Subgraph};
use crate::Error;

use graph_derive::recursive_graph;

#[recursive_graph]
mod expr {
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

    #[derive(Debug)]
    enum IntExpr {
        Int(i64),
        //Indirect(IndirectTest),
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

use expr::live::IntExpr as LiveIntExpr;
use expr::live_selector::BoolExpr as LiveExpr;
use expr::selector::BoolExpr as IntSelector;
use expr::storage::{BoolExpr, FloatExpr, IntExpr};

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
            IntSelector::IntExpr(IntExpr::Int(5)),
            IntSelector::IntExpr(IntExpr::Int(15)),
            IntSelector::IntExpr(IntExpr::Add(2.into(), 1.into())),
            IntSelector::IntExpr(IntExpr::Int(10)),
            IntSelector::IntExpr(IntExpr::Sub(2.into(), 1.into())),
        ])?;

        let root = expr.root();

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
