use typed_dag::{Arena, Visitable};
use typed_dag_derive::typed_dag;

#[typed_dag]
mod peano {
    enum Number {
        Zero,
        Successor(Number),
    }
}

impl peano::Number {
    fn new(val: u8) -> Arena<peano::container::Number, peano::Number> {
        Arena::build(|arena| {
            let mut a = arena.push(peano::Number::Zero);
            for _ in 0..val {
                a = arena.push(peano::Number::Successor(a));
            }
            a
        })
    }
}

impl<'view, V: peano::visitor::Number<'view>> peano::Number<V> {
    fn value(&self) -> usize {
        match self {
            peano::Number::Zero => 0,
            peano::Number::Successor(prev) => prev.expand().value() + 1,
        }
    }
}

#[test]
fn call_instance_method() {
    let three = peano::Number::new(3);
    let root_node: peano::Number<_> = three.visit_root().expand();
    assert_eq!(root_node.value(), 3);
}
