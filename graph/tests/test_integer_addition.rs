use graph::{GenericGraphNode, Graph, Live};
use graph_derive::recursive_graph;

#[recursive_graph]
mod expr {
    enum IntExpr {
        Int(i64),
        Add(IntExpr, IntExpr),
    }
}

// TODO: Should the Graph<T> container be an internal implementation
// detail?  A user probably should be able to define methods for the
// container holding their type.
// impl<'a> Graph<'a, expr::IntExpr<'a, Storage<'a>>> {
//     // TODO: Re-investigate whether this could be implemented with
//     // `std::ops::Deref`, to avoid needing to forward lots of function
//     // definitions to the top level node.
//     fn eval(&self) -> i64 {
//         self.borrow_root().eval()
//     }
// }

impl<'a, BaseType: GenericGraphNode<'a>> expr::IntExpr<'a, Live<'a, BaseType>>
where
    BaseType::DefaultSelector: graph::ContainerOf<'a, expr::IntExpr<'a>>,
{
    fn eval(&self) -> i64 {
        use expr::IntExpr::*;
        // TODO: Should LiveGraphRef implement Copy/Clone, so that the
        // double-dereference can be avoided?
        match self {
            Int(val) => **val,
            Add(a, b) => a.borrow().unwrap().eval() + b.borrow().unwrap().eval(),
        }
    }
}

#[test]
fn eval() {
    // TODO: This "use" statement is rather clunky.  Maybe these
    // should be method implementations on `Graph<T>`, or a standalone
    // class?
    let expression = {
        use expr::builder::*;
        let mut builder = Graph::new();
        let a = builder.Int(5);
        let b = builder.Int(10);
        let _c = builder.Add(a, b);
        builder
    };
    let root_node: expr::IntExpr<_> = expression.borrow_root().unwrap();
    assert_eq!(root_node.eval(), 15);
}
