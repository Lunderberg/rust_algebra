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
    fn new(val: u8) -> Arena<peano::default_container::Number, peano::Number> {
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
    // Version 1.  Pattern matching is done on the existing
    // struct.  If recursion is required, then "prev", a
    // VisitingRef, can be expanded into an instance of Self, and
    // can be called recursively.
    fn value_from_ref(&self) -> usize {
        match &self {
            peano::Number::Zero => 0,
            peano::Number::Successor(prev) => prev.expand().value_from_ref() + 1,
        }
    }

    // Version 2.  The recursive definitions are expanded first.
    // Here, "prev" is an instance of Self (though possibly with a
    // narrowing lifetime), and the `.value()` method can be
    // called directly.
    fn value_from_expand(&self) -> usize {
        match self.expand() {
            peano::Number::Zero => 0,
            peano::Number::Successor(prev) => prev.value_from_expand() + 1,
        }
    }

    // Version 3.  To see if I can go to sillier levels of
    // recursion.  Here, I expand up to two levels of the nested
    // structure.  Because madness.  This works because, after
    // type inference, the outer `peano::Number` has type
    // `peano::Number<NestedRef<V>>`, while the inner has type
    // `peano::Number<V>`.
    fn value_from_double_expand(&self) -> usize {
        return match self.expand().expand() {
            peano::Number::Zero => 0,
            peano::Number::Successor(peano::Number::Zero) => 1,
            peano::Number::Successor(peano::Number::Successor(prev)) => {
                prev.value_from_double_expand() + 2
            }
        };
    }
}

#[test]
fn call_instance_method() {
    let three = peano::Number::new(3);
    let root_node: peano::Number<_> = three.visit_root().expand();
    assert_eq!(root_node.value_from_ref(), 3);
    assert_eq!(root_node.value_from_expand(), 3);
    assert_eq!(root_node.value_from_double_expand(), 3);
}
