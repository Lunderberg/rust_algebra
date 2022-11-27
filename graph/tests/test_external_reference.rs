use graph::{GenericGraphNode, Graph, Live};
use graph_derive::recursive_graph;

#[recursive_graph]
mod expr {
    enum IntExpr<'a> {
        Int(&'a i64),
        Add(IntExpr, IntExpr),
    }
}

impl<'a: 'view, 'view, BaseType: GenericGraphNode<'view>>
    expr::IntExpr<'a, 'view, Live<'view, BaseType>>
where
    BaseType::DefaultSelector: graph::ContainerOf<'a, expr::IntExpr<'a, 'view>>,
{
    fn eval(&self) -> i64 {
        use expr::IntExpr::*;
        match self {
            Int(val) => ***val,
            Add(a, b) => a.borrow().unwrap().eval() + b.borrow().unwrap().eval(),
        }
    }
}

#[test]
fn eval() {
    let data: Vec<i64> = vec![5, 10];
    let expression = {
        use expr::builder::*;
        let mut builder = Graph::new();
        let a = builder.Int(&data[0]);
        let b = builder.Int(&data[1]);
        let _c = builder.Add(a, b);
        builder
    };
    let root_node: expr::IntExpr<_> = expression.borrow_root().unwrap();
    assert_eq!(root_node.eval(), 15);
}
