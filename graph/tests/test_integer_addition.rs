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
    let root_node: expr::IntExpr<_> = expression.root().borrow().unwrap();
    assert_eq!(root_node.eval(), 15);
}
