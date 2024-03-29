use typed_dag::{Arena, Visitable};
use typed_dag_derive::typed_dag;

#[typed_dag]
mod expr {
    enum IntExpr {
        Int(i64),
        Add(IntExpr, IntExpr),
    }
}

impl<'view, V: expr::visitor::IntExpr<'view>> expr::IntExpr<V> {
    fn eval(&self) -> i64 {
        use expr::IntExpr::*;
        match self.expand() {
            Int(val) => *val,
            Add(Int(a), Int(b)) => a + b,
            Add(a, b) => a.eval() + b.eval(),
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
    let root_node: expr::IntExpr<_> = expression.visit_root().expand();
    assert_eq!(root_node.eval(), 15);
}
