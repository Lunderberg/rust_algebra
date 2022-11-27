#![allow(unused_imports)]

use graph_derive::recursive_graph;

#[recursive_graph]
mod expr {
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

    // #[derive(Debug)]
    enum IntExpr {
        Int(i64),
        Add(IntExpr, IntExpr),
        Sub(IntExpr, IntExpr),
    }

    // #[derive(Debug)]
    enum FloatExpr {
        Float(f64),
        Add(FloatExpr, FloatExpr),
        Sub(FloatExpr, FloatExpr),
    }

    // #[derive(Debug)]
    enum BoolExpr {
        Bool(bool),
        IntEqual(IntExpr, IntExpr),
        FloatEqual(FloatExpr, FloatExpr),
        And(BoolExpr, BoolExpr),
        Or(BoolExpr, BoolExpr),
    }
}

use expr::{BoolExpr, IntExpr};

mod temp {
    use super::{BoolExpr, IntExpr};

    pub trait ContainerOf<'a, NodeType: graph::GenericGraphNode<'a, graph::Storage<'a>>> {
        fn to_container(node: NodeType) -> Self;
        fn from_container(&'a self) -> Result<&'a NodeType, graph::Error>;
    }

    impl<'a> ContainerOf<'a, BoolExpr<'a>> for super::expr::selector::BoolExpr<'a> {
        fn to_container(node: BoolExpr<'a>) -> Self {
            Self::BoolExpr(node)
        }

        fn from_container(&'a self) -> Result<&'a BoolExpr<'a>, graph::Error> {
            match self {
                Self::BoolExpr(expr) => Ok(expr),
                _ => Err(graph::Error::IncorrectType {
                    expected: "BoolExpr",
                    actual: "Something else",
                }),
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::Error;
    use graph::Graph;

    #[test]
    fn test_graph_build_macro() -> Result<(), Error> {
        let _expr_from_builder_explicit_types = {
            use super::expr::builder::*;
            let mut builder: Graph<BoolExpr<_>> = Graph::new();
            let a = builder.IntExpr_Int(5);
            let b = builder.IntExpr_Int(15);
            let c = builder.IntExpr_Add(a, b);
            let d = builder.IntExpr_Int(20);
            let e = builder.BoolExpr_IntEqual(c, d);
            let f = builder.FloatExpr_Float(5.0);
            let g = builder.FloatExpr_Float(15.0);
            let h = builder.FloatExpr_Add(f, g);
            let i = builder.FloatExpr_Float(20.0);
            let j = builder.BoolExpr_FloatEqual(h, i);
            let _k = builder.BoolExpr_And(e, j);
            builder
        };

        let _expr_from_builder_implicit_types = {
            use super::expr::builder::*;
            let mut builder: Graph<BoolExpr<_>> = Graph::new();
            let a = builder.Int(5);
            let b = builder.Int(15);
            let c = builder.Add(a, b);
            let d = builder.Int(20);
            let e = builder.IntEqual(c, d);
            let f = builder.Float(5.0);
            let g = builder.Float(15.0);
            let h = builder.Add(f, g);
            let i = builder.Float(20.0);
            let j = builder.FloatEqual(h, i);
            let _k = builder.And(e, j);
            builder
        };

        // println!("Expr from builder: {_expr_from_builder_implicit_types:#?}");

        // TODO: Actually perform a test here.  Will need a comparison
        // of storage contents.

        Ok(())
    }

    #[test]
    fn test_basic() -> Result<(), Error> {
        // let expr: Graph<Expr> = Graph::new(Expr::Sub(
        //     Expr::Add(Expr::Int(5), Expr::Int(15)),
        //     Expr::Int(10),
        // ));

        let expr: Graph<BoolExpr<_>> = {
            use super::expr::builder::*;
            let mut builder = Graph::new();
            let a = builder.IntExpr_Int(5);
            let b = builder.IntExpr_Int(15);
            let c = builder.IntExpr_Add(a, b);
            let d = builder.IntExpr_Int(10);
            builder.IntExpr_Sub(c, d);
            builder
        };

        //let root = expr.borrow_root()?;

        use graph::{Live, Storage};
        let root = expr.borrow_root::<IntExpr<_>>()?;
        //let root = expr.borrow_root::<IntExpr<Live<BoolExpr<Storage>>>>()?;

        // use graph::{Live, Storage};
        // let root: Result<IntExpr<Live<BoolExpr<Storage>>>, _> = expr.borrow_root();
        // let root = root?;

        // println!("Found int expression, {root:?}");
        // match root {
        //     IntExpr::Int(a) => {
        //         println!("IntLiteral {a:?}")
        //     }
        //     IntExpr::Add(a, b) => {
        //         println!("Addition of {a:?} and {b:?}")
        //     }
        //     IntExpr::Sub(a, b) => {
        //         println!(
        //             "Subtraction of {a:?} and {b:?}, which are {:?} and {:?}",
        //             a.borrow()?,
        //             b.borrow()?
        //         )
        //     }
        // }
        match root {
            IntExpr::Int(_a) => {}
            IntExpr::Add(_a, _b) => {}
            IntExpr::Sub(_a, _b) => {}
        }

        Ok(())
    }
}
