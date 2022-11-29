// use graph::{GenericGraphNode, Graph, Live};
// use graph_derive::recursive_graph;

#![allow(dead_code)]

mod graph2 {
    use std::marker::PhantomData;

    ///////////////////////////////////////////////////////////
    ////////////// Enable self-referential types //////////////
    ///////////////////////////////////////////////////////////

    /// Usage type (e.g. Storage, Builder, Visitor)
    pub trait RecursiveRefType {
        type Ref<T: ?Sized>;
    }

    /// Meta-object, at compile-time can generate an enum for a
    /// specific usage type.  At runtime, can convert between enums
    /// of different usage types.
    pub trait RecursiveObj {
        type Obj<R: RecursiveRefType>;

        fn convert<'a, OldRef: RecursiveRefType, NewRef: RecursiveRefType + 'a>(
            old_obj: &'a Self::Obj<OldRef>,
            converter: &impl RefTypeConverter<OldRef, NewRef>,
        ) -> Self::Obj<NewRef>
        where
            Self::Obj<NewRef>: 'a;
    }

    /// Convert between references of different usage types
    /// (e.g. build a Storage enum from a Builder enum, or build a
    /// Visitor enum from a Storage enum)
    pub trait RefTypeConverter<OldRef: RecursiveRefType, NewRef: RecursiveRefType> {
        fn convert_reference<T>(&self, old_ref: &OldRef::Ref<T>) -> NewRef::Ref<T>;
    }

    /// Utility reference type, used to avoid infinite recursion at
    /// compile-time.  This makes the types in the converter easier,
    /// because both old and new types share the same internal
    /// structures.  Converting `OldRef::Ref<Enum<NilRefType>>` to
    /// `NewRef::Ref<Enum<NilRefType>>` only requires changing the
    /// outer type, whereas converting `OldRef::Ref<Enum<OldRef>>` to
    /// `NewRef::Ref<Enum<NewRef>>` would also require converting the
    /// inner reference.
    pub struct NilRefType;
    impl RecursiveRefType for NilRefType {
        type Ref<T: ?Sized> = ();
    }

    /////////////////////////////////////
    ////////////// Storage //////////////
    /////////////////////////////////////

    /// A usage annotation for objects that may be stored in the
    /// linearized structure.
    pub struct Storage;

    impl RecursiveRefType for Storage {
        type Ref<T: ?Sized> = StorageRef<T>;
    }

    /// Represents a reference in the linearized structure.
    pub struct StorageRef<T: ?Sized> {
        /// Position of the referred-to type, relative to the position
        /// of the `RecursiveObj::Obj<Storage>` that holds the
        /// `StorageRef`.
        rel_pos: usize,
        _node: PhantomData<*const T>,
    }

    ///////////////////////////////////
    ////////////// Owner //////////////
    ///////////////////////////////////

    /// A container for an entire tree structure.  The container must
    /// be able to represent any individual node type that may occur
    /// within the tree.  These should be expected by implementing
    /// `ContainerOf<Node>` for each type that may be contained.
    pub struct Owner<Container> {
        nodes: Vec<Container>,
    }

    /// Exposes a type `T` as being potentially stored in a container
    /// `Container`.
    ///
    /// This is effectively the same as `Container: From<T> +
    /// TryInto<T>`, and may be replaced in the future, if that works
    /// with type inference and doesn't require extra bounds to be
    /// specified on the user side.
    ///
    /// - `trait_alias`: https://github.com/rust-lang/rust/issues/41517
    /// - `implied_bounds`: https://github.com/rust-lang/rust/issues/44491
    /// - `provide_any`: https://github.com/rust-lang/rust/issues/96024
    pub trait ContainerOf<T> {
        type Error;
        fn to_container(val: T) -> Self;
        fn from_container<'a>(&'a self) -> Result<&'a T, Self::Error>;
    }

    /// Inverse of `ContainerOf`, marks a node type as being stored
    /// inside a specific `Container`.  Automatically implemented in
    /// terms of `ContainerOf`.
    pub trait ContainedBy<Container> {
        type Error;
        fn to_container(self) -> Container;
        fn from_container<'a>(container: &'a Container) -> Result<&'a Self, Self::Error>;
    }

    impl<T, Container: ContainerOf<T>> ContainedBy<Container> for T {
        type Error = Container::Error;
        fn to_container(self) -> Container {
            Container::to_container(self)
        }
        fn from_container<'a>(container: &'a Container) -> Result<&'a Self, Self::Error> {
            container.from_container()
        }
    }

    /////////////////////////////////////
    ////////////// Builder //////////////
    /////////////////////////////////////

    /// A constructor used to generate a `Owner<Container>`.
    pub struct Builder<Container> {
        output_graph: Owner<Container>,
    }

    impl<Container> Builder<Container> {
        /// Constructs an empty `Builder`.
        pub fn new() -> Self {
            let output_graph = Owner { nodes: Vec::new() };
            Self { output_graph }
        }
    }

    /// Reference type used while building a tree.  Any time the user
    /// pushes a node into the builder, they receive a reference.
    /// That reference may then be used to construct additional
    /// builder nodes.
    pub struct BuilderRef<T: ?Sized> {
        abs_pos: usize,
        _node: PhantomData<*const T>,
    }

    impl<Container> RecursiveRefType for Builder<Container> {
        type Ref<T: ?Sized> = BuilderRef<T>;
    }

    impl<Container> Builder<Container> {
        /// Insert a new node to the builder
        pub fn push<R: RecursiveObj<Obj<Builder<Container>> = BuilderT>, BuilderT>(
            &mut self,
            builder: BuilderT,
        ) -> BuilderRef<R::Obj<NilRefType>>
        where
            R::Obj<Storage>: ContainedBy<Container>,
        {
            let abs_pos = self.output_graph.nodes.len();
            let converter = BuilderToStorage { size: abs_pos };
            let storage: R::Obj<Storage> = R::convert(&builder, &converter);
            let container: Container = storage.to_container();
            self.output_graph.nodes.push(container);
            BuilderRef {
                abs_pos,
                _node: PhantomData,
            }
        }
    }

    ////////////////////////////////////////
    ////////////// Converters //////////////
    ////////////////////////////////////////

    /// Converts from Builder references using absolute position to
    /// Storage references using relative position.
    struct BuilderToStorage {
        size: usize,
    }

    impl<Container> RefTypeConverter<Builder<Container>, Storage> for BuilderToStorage {
        fn convert_reference<T>(&self, old_ref: &BuilderRef<T>) -> StorageRef<T> {
            let rel_pos = self
                .size
                .checked_sub(old_ref.abs_pos)
                .expect("Invalid reference type");
            StorageRef {
                rel_pos,
                _node: PhantomData,
            }
        }
    }
}

pub mod peano {
    // Initial definition
    // enum Number {
    //     Zero,
    //     Successor(Number),
    // }

    use super::graph2::*;

    pub enum Number<R: RecursiveRefType> {
        Zero,
        Successor(R::Ref<Number<NilRefType>>),
    }

    pub struct NumberObj;

    impl RecursiveObj for NumberObj {
        type Obj<R: RecursiveRefType> = Number<R>;

        fn convert<'a, OldRef: RecursiveRefType, NewRef: RecursiveRefType + 'a>(
            old_obj: &'a Self::Obj<OldRef>,
            converter: &impl RefTypeConverter<OldRef, NewRef>,
        ) -> Self::Obj<NewRef>
        where
            Self::Obj<NewRef>: 'a,
        {
            match old_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => {
                    Number::Successor(converter.convert_reference(old_ref))
                }
            }
        }
    }

    pub enum NumberContainer {
        Number(Number<Storage>),
    }

    impl ContainerOf<Number<Storage>> for NumberContainer {
        type Error = graph::Error;
        fn to_container(val: Number<Storage>) -> Self {
            Self::Number(val)
        }
        fn from_container<'a>(&'a self) -> Result<&'a Number<Storage>, Self::Error> {
            match self {
                Self::Number(val) => Ok(val),
            }
        }
    }
}

// impl<'a> peano::Number<'a> {
//     fn new(val: u8) -> Graph<'a, Self> {
//         use peano::builder::*;
//         let mut builder = Graph::new();
//         let mut a = builder.Zero();
//         for _ in 0..val {
//             a = builder.Successor(a);
//         }
//         builder
//     }
// }

// impl<'a, BaseType> peano::Number<'a, Live<'a, BaseType>>
// where
//     BaseType: GenericGraphNode<'a>,
//     BaseType::DefaultSelector: graph::ContainerOf<'a, peano::Number<'a>>,
// {
//     fn value(&self) -> usize {
//         match self {
//             peano::Number::Zero => 0,
//             peano::Number::Successor(live_ref) => {
//                 let num = live_ref.borrow().unwrap();
//                 num.value() + 1
//             }
//         }
//     }
// }

#[test]
fn construct_annotated() {
    let _three = {
        let mut builder = graph2::Builder::<peano::NumberContainer>::new();
        let mut a: graph2::BuilderRef<peano::Number<_>> = builder.push::<peano::NumberObj, _>(
            peano::Number::<graph2::Builder<peano::NumberContainer>>::Zero,
        );
        for _ in 0..3 {
            a = builder.push::<peano::NumberObj, _>(peano::Number::<
                graph2::Builder<peano::NumberContainer>,
            >::Successor(a));
        }
        builder
    };
}

#[test]
fn construct_unannotated() {
    let _three = {
        let mut builder = graph2::Builder::<peano::NumberContainer>::new();
        let mut a = builder.push::<peano::NumberObj, _>(peano::Number::Zero);
        for _ in 0..3 {
            a = builder.push::<peano::NumberObj, _>(peano::Number::Successor(a));
        }
        builder
    };
}

// #[test]
// fn construct_unannotated() {
//     let _three = {
//         let mut builder = graph2::Builder::<peano::NumberContainer>::new();
//         let mut a = builder.push(peano::Number::Zero);
//         for _ in 0..3 {
//             a = builder.push(peano::Number::Successor(a));
//         }
//         builder
//     };
// }

// #[test]
// fn call_static_method() {
//     let _three = peano::Number::new(3);
// }

// #[test]
// fn call_instance_method() {
//     let three = peano::Number::new(3);
//     let root_node: peano::Number<_> = three.borrow_root().unwrap();
//     assert_eq!(root_node.value(), 3);
// }
