use graph::{Arena, Visitable};
use graph_derive::recursive_graph;

#[recursive_graph]
mod expr {
    enum IntExpr {
        Int(i64),
        Add(IntExpr, IntExpr),
    }
}

impl<'view, V: expr::visitor::IntExpr<'view>> expr::IntExpr<V> {
    fn eval(&self) -> i64 {
        match self {
            Self::Int(val) => **val,
            Self::Add(a, b) => a.borrow().eval() + b.borrow().eval(),
        }
    }
}

#[test]
fn eval() {
    use expr::IntExpr;
    let expression = Arena::build(|arena| {
        let a = arena.push(IntExpr::Int(5));
        let b = arena.push(IntExpr::Int(10));
        let c = arena.push(IntExpr::Add(a, b));
        c
    });
    let root_node: expr::IntExpr<_> = expression.visit_root().borrow();
    assert_eq!(root_node.eval(), 15);
}
