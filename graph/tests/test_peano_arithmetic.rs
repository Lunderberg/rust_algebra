use graph::{Builder, ContainerOf, TypedTree, Visiting};
use graph_derive::recursive_graph;

#[recursive_graph]
mod peano {
    enum Number {
        Zero,
        Successor(Number),
    }
}

impl<'a> peano::Number<'a> {
    fn new(val: u8) -> TypedTree<'a, Self> {
        let mut builder = Builder::new();
        let mut a = builder.push(peano::Number::Zero);
        for _ in 0..val {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.into()
    }
}

// // TODO: Simplify the "impl", somehow.  Requiring two generic types,
// // specific Storage/Live/DefaultSelector implementations, and multiple
// // "where" clauses would be completely unreadable and unusable to a
// // user.
impl<'a, Container: ContainerOf<'a, peano::Number<'a>>> peano::Number<'a, Visiting<'a, Container>> {
    fn value(&self) -> usize {
        match self {
            peano::Number::Zero => 0,
            peano::Number::Successor(live_ref) => {
                let num = live_ref.borrow().unwrap();
                num.value() + 1
            }
        }
    }
}

#[test]
fn construct() {
    let _three: TypedTree<'_, peano::Number> = {
        let mut builder = Builder::new();
        let mut a = builder.push(peano::Number::Zero);
        for _ in 0..3 {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.into()
    };
}

#[test]
fn call_static_method() {
    let _three = peano::Number::new(3);
}

#[test]
fn call_instance_method() {
    let three = peano::Number::new(3);
    let root_node: peano::Number<_> = three.borrow().unwrap();
    assert_eq!(root_node.value(), 3);
}
