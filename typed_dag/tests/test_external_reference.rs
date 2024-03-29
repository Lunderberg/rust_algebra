use typed_dag::{Arena, Visitable};
use typed_dag_derive::typed_dag;

#[typed_dag]
mod expr {
    enum IntExpr<'a> {
        Int(&'a i64),
        Add(IntExpr<'a>, IntExpr<'a>),
    }
}
impl<'a, V: expr::visitor::IntExpr<'a>> expr::IntExpr<'a, V> {
    fn eval(&self) -> i64 {
        use expr::IntExpr::*;
        match self.expand() {
            Int(val) => **val,
            Add(a, b) => a.eval() + b.eval(),
        }
    }
}

#[test]
fn eval() {
    use expr::IntExpr;

    let data: Vec<i64> = vec![5, 10];
    let expression = Arena::build(|arena| {
        let a = arena.push(IntExpr::Int(&data[0]));
        let b = arena.push(IntExpr::Int(&data[1]));
        let c = arena.push(IntExpr::Add(a, b));
        c
    });
    let root_node: expr::IntExpr<_> = expression.visit_root().expand();
    assert_eq!(root_node.eval(), 15);
}
