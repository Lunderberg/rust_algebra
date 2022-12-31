// use graph::{ContainerOf, Visiting};
// use graph_derive::recursive_graph;
use graph::{Builder, ContainerOf, TypedTree, Visiting};
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
// impl<'a> TypedTree<'a, expr::IntExpr<'a>> {
//     // TODO: Re-investigate whether this could be implemented with
//     // `std::ops::Deref`, to avoid needing to forward lots of function
//     // definitions to the top level node.
//     fn eval(&self) -> i64 {
//         self.borrow_root().eval()
//     }
// }

impl<'a, Container: ContainerOf<expr::IntExpr<'a>>> expr::IntExpr<'a, Visiting<'a, Container>> {
    fn eval(&self) -> i64 {
        match self {
            Self::Int(val) => **val,
            Self::Add(a, b) => a.borrow().unwrap().eval() + b.borrow().unwrap().eval(),
        }
    }
}

#[test]
fn eval() {
    use expr::IntExpr;
    let expression: TypedTree<IntExpr> = {
        let mut builder = Builder::new();
        let a = builder.push(IntExpr::Int(5));
        let b = builder.push(IntExpr::Int(10));
        let _c = builder.push(IntExpr::Add(a, b));
        builder.into()
    };
    let root_node: expr::IntExpr<_> = expression.borrow().unwrap();
    assert_eq!(root_node.eval(), 15);
}
