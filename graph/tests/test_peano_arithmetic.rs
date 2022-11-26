use graph::{GenericGraphNode, Graph, Live, Storage};
use graph_derive::recursive_graph;

#[recursive_graph]
mod peano {
    enum Number {
        Zero,
        Successor(Number),
    }
}

impl<'a> peano::Number<'a, Storage<'a>> {
    fn new(val: u8) -> Graph<'a, Self> {
        use peano::builder::*;
        let mut builder = Graph::new();
        let mut a = builder.Zero();
        for _ in 0..val {
            a = builder.Successor(a);
        }
        builder
    }
}

// TODO: Simplify the "impl", somehow.  Requiring two generic types,
// specific Storage/Live/DefaultSelector implementations, and multiple
// "where" clauses would be completely unreadable and unusable to a
// user.
impl<'a, BaseType> peano::Number<'a, Live<'a, BaseType>>
where
    BaseType: GenericGraphNode<'a, Storage<'a>>,
    &'a peano::Number<'a, Storage<'a>>:
        TryFrom<&'a BaseType::DefaultSelector, Error = graph::Error>,
    // TODO: Why doesn't the equivalent TryInto trait allow
    // node.borrow() to be called?
    //
    //&'a BaseType::DefaultSelector: TryInto<&'a peano::Number<'a, Storage<'a>>>,
{
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
    let _three = {
        use peano::builder::*;
        let mut builder = Graph::new();
        let mut a = builder.Zero();
        for _ in 0..3 {
            a = builder.Successor(a);
        }
        builder
    };
}

#[test]
fn call_static_method() {
    let _three = peano::Number::new(3);
}

#[test]
fn call_instance_method() {
    let three = peano::Number::new(3);
    let root_node: peano::Number<_> = three.borrow_root().unwrap();
    assert_eq!(root_node.value(), 3);
}
