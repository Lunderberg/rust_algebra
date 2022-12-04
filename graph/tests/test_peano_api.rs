// use graph::{GenericGraphNode, Graph, Live};
// use graph_derive::recursive_graph;

#![allow(dead_code)]

mod graph2 {
    use std::marker::PhantomData;

    ///////////////////////////////////////////////////////////
    ////////////// Enable self-referential types //////////////
    ///////////////////////////////////////////////////////////

    /// Usage type (e.g. Storage, Builder, Visitor)
    pub trait RecursiveRefType<'a>: 'a {
        type Ref<T: ?Sized>;
    }

    /// Meta-object, at compile-time can generate an enum for a
    /// specific usage type.  At runtime, can convert between enums
    /// of different usage types.
    pub trait RecursiveFamily<'a>: 'a {
        type Obj<R: RecursiveRefType<'a>>: RecursiveObj<'a, RefType = R, Family = Self> + 'a;

        type DefaultContainer: 'a;

        fn convert<OldRef: RecursiveRefType<'a>, NewRef: RecursiveRefType<'a>>(
            old_obj: &Self::Obj<OldRef>,
            converter: &impl RefTypeConverter<'a, OldRef, NewRef>,
        ) -> Self::Obj<NewRef>;
    }

    /// A recursive object, belonging to a specific family of
    /// recursive types, with a specific type in that family.  This
    /// trait is automatically implemented, and is used for functions
    /// that must derive their types from arguments, and return
    /// another type in the same family.  (e.g. `Builder::push`
    /// accepts an argument of type `F::Obj<Builder>` and must
    /// internally convert it to an object of type `F::Obj<Storage>`)
    pub trait RecursiveObj<'a>: 'a {
        type Family: RecursiveFamily<'a>;
        type RefType: RecursiveRefType<'a>;
    }

    /// Convert between references of different usage types
    /// (e.g. build a Storage enum from a Builder enum, or build a
    /// Visitor enum from a Storage enum)
    pub trait RefTypeConverter<'a, OldRef: RecursiveRefType<'a>, NewRef: RecursiveRefType<'a>> {
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
    impl<'a> RecursiveRefType<'a> for NilRefType {
        type Ref<T: ?Sized> = ();
    }

    /////////////////////////////////////
    ////////////// Storage //////////////
    /////////////////////////////////////

    /// A usage annotation for objects that may be stored in the
    /// linearized structure.
    pub struct Storage;

    impl<'a> RecursiveRefType<'a> for Storage {
        type Ref<T: ?Sized> = StorageRef<T>;
    }

    /// Represents a reference in the linearized structure.
    pub struct StorageRef<T: ?Sized> {
        /// Position of the referred-to type, relative to the position
        /// of the `RecursiveFamily::Obj<Storage>` that holds the
        /// `StorageRef`.
        rel_pos: usize,
        _node: PhantomData<*const T>,
    }

    ///////////////////////////////////////
    ////////////// TypedTree //////////////
    ///////////////////////////////////////

    /// A container for an entire tree structure.  The container must
    /// be able to represent any individual node type that may occur
    /// within the tree.  These should be expected by implementing
    /// `ContainerOf<Node>` for each type that may be contained.
    pub struct TypedTree<'a,
        RootNodeType: RecursiveObj<'a>,
        Container = <<RootNodeType as RecursiveObj<'a>>::Family as RecursiveFamily<'a>>::DefaultContainer,
    > {
        nodes: Vec<Container>,
        _phantom: PhantomData<*const RootNodeType>,
        _a: PhantomData<&'a usize>,
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
    pub trait ContainerOf<'a, R: RecursiveObj<'a>>: 'a {
        fn to_container(val: <R::Family as RecursiveFamily<'a>>::Obj<Storage>) -> Self;
        fn from_container(
            &'a self,
        ) -> Result<&'a <R::Family as RecursiveFamily<'a>>::Obj<Storage>, graph::Error>;
    }

    /// Inverse of `ContainerOf`, marks a node type as being stored
    /// inside a specific `Container`.  Automatically implemented in
    /// terms of `ContainerOf`.
    pub trait ContainedBy<'a, Container> {
        fn to_container(self) -> Container;
        fn from_container(container: &'a Container) -> Result<&'a Self, graph::Error>;
    }

    impl<
            'a,
            F: RecursiveFamily<'a>,
            T: RecursiveObj<'a, Family = F, RefType = Storage>,
            Container: ContainerOf<'a, T>,
        > ContainedBy<'a, Container> for T
    where
        F: RecursiveFamily<'a, Obj<Storage> = T>,
    {
        fn to_container(self) -> Container {
            Container::to_container(self)
        }
        fn from_container(container: &'a Container) -> Result<&'a Self, graph::Error> {
            container.from_container()
        }
    }

    impl<'a, F: RecursiveFamily<'a>, RootNodeType: RecursiveObj<'a, Family = F>, Container>
        TypedTree<'a, RootNodeType, Container>
    where
        Container: ContainerOf<'a, RootNodeType>,
    {
        pub fn borrow(&'a self) -> Result<F::Obj<Visiting<'a, Container>>, graph::Error> {
            let container: &Container = self.nodes.last().unwrap();
            let node: &F::Obj<Storage> = container.from_container()?;
            let converter = StorageToVisiting { view: &self.nodes };
            let live_ref = F::convert(node, &converter);
            Ok(live_ref)
        }
    }

    /////////////////////////////////////
    ////////////// Builder //////////////
    /////////////////////////////////////

    /// A constructor used to generate a `TypedTree<Container>`.
    pub struct Builder<'a, Container: 'a> {
        nodes: Vec<Container>,
        _a: PhantomData<&'a usize>,
    }

    impl<'a, Container> Builder<'a, Container> {
        /// Constructs an empty `Builder`.
        pub fn new() -> Self {
            Self {
                nodes: Vec::new(),
                _a: PhantomData,
            }
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

    impl<'a, Container> RecursiveRefType<'a> for Builder<'a, Container> {
        type Ref<T: ?Sized> = BuilderRef<T>;
    }

    impl<'a, Container> Builder<'a, Container> {
        /// Insert a new node to the builder
        pub fn push<
            F: RecursiveFamily<'a>,
            T: RecursiveObj<'a, RefType = Builder<'a, Container>, Family = F>,
        >(
            &mut self,
            builder_obj: T,
        ) -> BuilderRef<F::Obj<NilRefType>>
        where
            F::Obj<Storage>: ContainedBy<'a, Container>,
            F: RecursiveFamily<'a, Obj<Builder<'a, Container>> = T>,
        {
            let abs_pos = self.nodes.len();
            let converter = BuilderToStorage { size: abs_pos };
            let storage_obj: F::Obj<Storage> = F::convert(&builder_obj, &converter);
            let container: Container = storage_obj.to_container();
            self.nodes.push(container);
            BuilderRef {
                abs_pos,
                _node: PhantomData,
            }
        }
    }

    impl<'a, RootNodeType: RecursiveObj<'a>, Container> From<Builder<'a, Container>>
        for TypedTree<'a, RootNodeType, Container>
    {
        fn from(builder: Builder<Container>) -> Self {
            Self {
                nodes: builder.nodes,
                _phantom: PhantomData,
                _a: PhantomData,
            }
        }
    }

    //////////////////////////////////////////
    //////////////   Visiting   //////////////
    //////////////////////////////////////////

    pub struct Visiting<'a, Container: 'a> {
        _a: PhantomData<&'a usize>,
        _c: PhantomData<*const Container>,
    }

    pub struct VisitingRef<'a, T: ?Sized, Container: 'a> {
        rel_pos: usize,
        view: &'a [Container],
        _phantom: PhantomData<*const T>,
    }

    impl<'a, Container> RecursiveRefType<'a> for Visiting<'a, Container> {
        type Ref<T: ?Sized> = VisitingRef<'a, T, Container>;
    }

    impl<
            'a,
            Family: RecursiveFamily<'a>,
            T: RecursiveObj<'a, Family = Family>,
            Container: ContainerOf<'a, T>,
        > VisitingRef<'a, T, Container>
    {
        pub fn borrow(&self) -> Result<Family::Obj<Visiting<'a, Container>>, graph::Error> {
            let self_index = self
                .view
                .len()
                .checked_sub(1)
                .ok_or(graph::Error::EmptyExpression)?;

            let index =
                self_index
                    .checked_sub(self.rel_pos)
                    .ok_or(graph::Error::InvalidReference {
                        rel_pos: self.rel_pos,
                        subgraph_size: self.view.len(),
                    })?;

            let container: &Container = &self.view[index];
            let node: &Family::Obj<Storage> = container.from_container()?;
            let converter = StorageToVisiting {
                view: &self.view[..=index],
            };
            let live_ref = Family::convert(node, &converter);
            Ok(live_ref)
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

    impl<'a, Container> RefTypeConverter<'a, Builder<'a, Container>, Storage> for BuilderToStorage {
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

    /// Converts from Storage references as they are stored in the
    /// contiguous array to a Visiting reference
    struct StorageToVisiting<'a, Container> {
        view: &'a [Container],
    }

    impl<'a, Container> RefTypeConverter<'a, Storage, Visiting<'a, Container>>
        for StorageToVisiting<'a, Container>
    {
        fn convert_reference<T>(&self, old_ref: &StorageRef<T>) -> VisitingRef<'a, T, Container> {
            VisitingRef {
                rel_pos: old_ref.rel_pos,
                view: self.view,
                _phantom: PhantomData,
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

    pub enum Number<'a, R: RecursiveRefType<'a> = NilRefType> {
        Zero,
        Successor(R::Ref<Number<'a, NilRefType>>),
    }

    pub struct NumberFamily;

    impl<'a> RecursiveFamily<'a> for NumberFamily {
        type Obj<R: RecursiveRefType<'a>> = Number<'a, R>;

        type DefaultContainer = NumberContainer<'a>;

        fn convert<OldRef: RecursiveRefType<'a>, NewRef: RecursiveRefType<'a>>(
            old_obj: &Self::Obj<OldRef>,
            converter: &impl RefTypeConverter<'a, OldRef, NewRef>,
        ) -> Self::Obj<NewRef> {
            match old_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => {
                    Number::Successor(converter.convert_reference(old_ref))
                }
            }
        }
    }

    impl<'a, RefType: RecursiveRefType<'a>> RecursiveObj<'a> for Number<'a, RefType> {
        type Family = NumberFamily;
        type RefType = RefType;
    }

    pub enum NumberContainer<'a> {
        Number(Number<'a, Storage>),
    }

    impl<'a, RefType: RecursiveRefType<'a>> ContainerOf<'a, Number<'a, RefType>>
        for NumberContainer<'a>
    {
        fn to_container(val: Number<'a, Storage>) -> Self {
            Self::Number(val)
        }
        fn from_container(&self) -> Result<&Number<'a, Storage>, graph::Error> {
            match self {
                Self::Number(val) => Ok(val),
            }
        }
    }
}

impl<'a> graph2::TypedTree<'a, peano::Number<'a>> {
    fn new(val: u8) -> Self {
        let mut builder = graph2::Builder::new();
        let mut a = builder.push(peano::Number::Zero);
        for _ in 0..val {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.into()
    }
}

impl<'a, Container: graph2::ContainerOf<'a, peano::Number<'a>>>
    peano::Number<'a, graph2::Visiting<'a, Container>>
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
fn construct_annotated() {
    let _three: graph2::TypedTree<peano::Number<graph2::Storage>, peano::NumberContainer> = {
        let mut builder = graph2::Builder::<peano::NumberContainer>::new();
        let mut a: graph2::BuilderRef<peano::Number<_>> = builder.push::<peano::NumberFamily, _>(
            peano::Number::<graph2::Builder<peano::NumberContainer>>::Zero,
        );
        for _ in 0..3 {
            a = builder.push::<peano::NumberFamily, _>(peano::Number::<
                graph2::Builder<peano::NumberContainer>,
            >::Successor(a));
        }
        builder.into()
    };
}

#[test]
fn construct_unannotated() {
    let _three: graph2::TypedTree<peano::Number> = {
        let mut builder = graph2::Builder::new();
        let mut a = builder.push(peano::Number::Zero);

        for _ in 0..3 {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.into()
    };
}

#[test]
fn call_static_method() {
    let _three = graph2::TypedTree::<peano::Number>::new(3);
}

#[test]
fn match_root() -> Result<(), graph::Error> {
    let zero = graph2::TypedTree::<peano::Number>::new(0);
    let one = graph2::TypedTree::<peano::Number>::new(1);

    use peano::Number;
    assert!(matches!(zero.borrow()?, Number::Zero));
    assert!(matches!(one.borrow()?, Number::Successor(_)));

    Ok(())
}

#[test]
fn match_live_ref() -> Result<(), graph::Error> {
    let three = graph2::TypedTree::<peano::Number>::new(3);

    use peano::Number;
    let value = match three.borrow()? {
        Number::Zero => 0,
        Number::Successor(a) => match a.borrow()? {
            Number::Zero => 1,
            Number::Successor(b) => match b.borrow()? {
                Number::Zero => 2,
                Number::Successor(c) => match c.borrow()? {
                    Number::Zero => 3,
                    Number::Successor(d) => match d.borrow()? {
                        Number::Zero => 4,
                        Number::Successor(_) => panic!("Recursed too var in match"),
                    },
                },
            },
        },
    };

    assert_eq!(value, 3);

    Ok(())
}

#[test]
fn call_instance_method() -> Result<(), graph::Error> {
    let three = graph2::TypedTree::<peano::Number>::new(3);
    let value = three.borrow()?.value();
    assert_eq!(value, 3);
    Ok(())
}
