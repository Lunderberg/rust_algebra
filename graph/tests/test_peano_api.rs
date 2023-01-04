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
        type Ref<'a, T: 'a + ?Sized>
        where
            Self: 'a;
        type Value<'a, T: 'a>;
    }

    /// Meta-object, at compile-time can generate an enum for a
    /// specific usage type.  At runtime, can convert between enums
    /// of different usage types.
    pub trait RecursiveFamily {
        type Obj<'a, R: RecursiveRefType + 'a>: RecursiveObj<'a, RefType = R, Family = Self>;

        fn builder_to_storage<'a>(
            builder_obj: Self::Obj<'a, Builder>,
            converter: BuilderToStorage,
        ) -> Self::Obj<'a, Storage>;

        fn storage_to_visiting<'a, Container>(
            storage_obj: &'a Self::Obj<'a, Storage>,
            converter: StorageToVisiting<'a, Container>,
        ) -> Self::Obj<'a, Visiting<'a, Container>>;
    }

    /// A recursive object, belonging to a specific family of
    /// recursive types, with a specific type in that family.  This
    /// trait is automatically implemented, and is used for functions
    /// that must derive their types from arguments, and return
    /// another type in the same family.  (e.g. `Builder::push`
    /// accepts an argument of type `F::Obj<Builder>` and must
    /// internally convert it to an object of type `F::Obj<Storage>`)
    pub trait RecursiveObj<'a>: 'a {
        type Family: RecursiveFamily<Obj<'a, Self::RefType> = Self>;
        type RefType: RecursiveRefType;
    }

    /////////////////////////////////////
    ////////////// Storage //////////////
    /////////////////////////////////////

    /// A usage annotation for objects that may be stored in the
    /// linearized structure.
    pub struct Storage;

    impl RecursiveRefType for Storage {
        type Ref<'a, T: 'a + ?Sized> = StorageRef<T>;
        type Value<'a, T: 'a> = T;
    }

    /// Represents a reference in the linearized structure.
    pub struct StorageRef<T: ?Sized> {
        /// Position of the referred-to type, relative to the position
        /// of the `RecursiveFamily::Obj<Storage>` that holds the
        /// `StorageRef`.
        rel_pos: usize,
        _node: PhantomData<*const T>,
    }

    impl<T: ?Sized> Clone for StorageRef<T> {
        fn clone(&self) -> Self {
            Self {
                rel_pos: self.rel_pos,
                _node: PhantomData,
            }
        }
    }

    ///////////////////////////////////////
    ////////////// TypedTree //////////////
    ///////////////////////////////////////

    pub trait Visitable<'a, 'b>: 'a {
        type RetType;

        fn borrow(&'b self) -> Result<Self::RetType, graph::Error>;
    }

    /// A container for an entire tree structure.  The container must
    /// be able to represent any individual node type that may occur
    /// within the tree.  These should be expected by implementing
    /// `ContainerOf<Node>` for each type that may be contained.
    pub struct TypedTree<RootNodeType, Container> {
        nodes: Vec<Container>,
        _phantom: PhantomData<*const RootNodeType>,
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
        fn to_container(val: T) -> Self;
        fn from_container(&self) -> Result<&T, graph::Error>;
    }

    /// Inverse of `ContainerOf`, marks a node type as being stored
    /// inside a specific `Container`.  Automatically implemented in
    /// terms of `ContainerOf`.
    pub trait ContainedBy<Container> {
        fn to_container(self) -> Container;
        fn from_container<'a>(container: &'a Container) -> Result<&'a Self, graph::Error>;
    }

    impl<T, Container: ContainerOf<T>> ContainedBy<Container> for T {
        fn to_container(self) -> Container {
            Container::to_container(self)
        }
        fn from_container<'a>(container: &'a Container) -> Result<&'a Self, graph::Error> {
            container.from_container()
        }
    }

    impl<RootNodeType, Container> TypedTree<RootNodeType, Container> {
        pub fn root(&self) -> VisitingRef<RootNodeType, Container> {
            VisitingRef {
                rel_pos: 0,
                view: &self.nodes,
                _phantom: PhantomData,
            }
        }
    }

    /////////////////////////////////////
    ////////////// Builder //////////////
    /////////////////////////////////////

    /// A constructor used to generate a `TypedTree<Container>`.
    pub struct Builder;

    pub struct BuilderObj<Container> {
        nodes: Vec<Container>,
    }

    impl Builder {
        /// Constructs an empty `Builder`.
        pub fn new<Container>() -> BuilderObj<Container> {
            BuilderObj { nodes: Vec::new() }
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

    impl RecursiveRefType for Builder {
        type Ref<'a, T: 'a + ?Sized> = BuilderRef<T>;
        type Value<'a, T: 'a> = T;
    }

    impl<Container> BuilderObj<Container> {
        /// Insert a new node to the builder
        pub fn push<'a, T: RecursiveObj<'a, RefType = Builder>>(
            &mut self,
            builder_obj: T,
        ) -> BuilderRef<<T::Family as RecursiveFamily>::Obj<'a, Storage>>
        where
            Container: ContainerOf<<T::Family as RecursiveFamily>::Obj<'a, Storage>>,
            T::Family: RecursiveFamily<Obj<'a, Builder> = T>,
        {
            let abs_pos = self.nodes.len();
            let converter = BuilderToStorage { size: abs_pos };
            let storage_obj =
                <T::Family as RecursiveFamily>::builder_to_storage(builder_obj, converter);
            let container: Container = storage_obj.to_container();
            self.nodes.push(container);
            BuilderRef {
                abs_pos,
                _node: PhantomData,
            }
        }
    }

    impl<RootNodeType, Container> From<BuilderObj<Container>> for TypedTree<RootNodeType, Container> {
        fn from(builder: BuilderObj<Container>) -> Self {
            Self {
                nodes: builder.nodes,
                _phantom: PhantomData,
            }
        }
    }

    //////////////////////////////////////////
    //////////////   Visiting   //////////////
    //////////////////////////////////////////

    pub struct Visiting<'a, Container> {
        _a: PhantomData<&'a [Container]>,
    }

    #[derive(Clone, Copy)]
    pub struct VisitingRef<'a, T: ?Sized, Container> {
        rel_pos: usize,
        view: &'a [Container],
        _phantom: PhantomData<*const T>,
    }

    impl<'b, Container> RecursiveRefType for Visiting<'b, Container> {
        type Ref<'a, T: 'a + ?Sized> = VisitingRef<'b, T, Container> where 'b: 'a;
        type Value<'a, T: 'a> = &'a T;
    }

    impl<
            'a: 'b,
            'b,
            NodeType: RecursiveObj<'a, RefType = Storage> + 'a,
            Container: ContainerOf<NodeType> + 'a,
        > Visitable<'a, 'b> for VisitingRef<'a, NodeType, Container>
    where
        Self: 'a,
    {
        type RetType = <NodeType::Family as RecursiveFamily>::Obj<'a, Visiting<'a, Container>>;

        fn borrow(&'b self) -> Result<Self::RetType, graph::Error> {
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
            let node: &NodeType = container.from_container()?;
            let converter = StorageToVisiting {
                view: &self.view[..=index],
            };
            let live_ref =
                <NodeType::Family as RecursiveFamily>::storage_to_visiting(node, converter);
            Ok(live_ref)
        }
    }

    ////////////////////////////////////////
    ////////////// Converters //////////////
    ////////////////////////////////////////

    /// Converts from Builder references using absolute position to
    /// Storage references using relative position.
    pub struct BuilderToStorage {
        size: usize,
    }

    impl BuilderToStorage {
        pub fn move_reference<T>(&self, old_ref: BuilderRef<T>) -> StorageRef<T> {
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
    pub struct StorageToVisiting<'a, Container> {
        view: &'a [Container],
    }

    impl<'a, Container> StorageToVisiting<'a, Container> {
        pub fn view_reference<T>(
            &self,
            storage_ref: &StorageRef<T>,
        ) -> VisitingRef<'a, T, Container> {
            VisitingRef {
                rel_pos: storage_ref.rel_pos,
                view: self.view,
                _phantom: PhantomData,
            }
        }
    }
}

use graph2::Visitable;

pub mod peano {
    // Initial definition
    // enum Number {
    //     Zero,
    //     Successor(Number),
    // }

    use super::graph2::*;

    pub enum Number<'a, R: RecursiveRefType + 'a = Storage> {
        Zero,
        Successor(R::Ref<'a, Number<'a, Storage>>),
    }

    pub struct NumberFamily;

    impl RecursiveFamily for NumberFamily {
        type Obj<'a, R: RecursiveRefType + 'a> = Number<'a, R>;

        fn builder_to_storage<'a>(
            builder_obj: Self::Obj<'a, Builder>,
            converter: BuilderToStorage,
        ) -> Self::Obj<'a, Storage> {
            match builder_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(converter.move_reference(old_ref)),
            }
        }

        fn storage_to_visiting<'a, Container>(
            storage_obj: &'a Self::Obj<'a, Storage>,
            converter: StorageToVisiting<'a, Container>,
        ) -> Self::Obj<'a, Visiting<'a, Container>> {
            match &storage_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(converter.view_reference(old_ref)),
            }
        }
    }

    impl<'a, RefType: RecursiveRefType> RecursiveObj<'a> for Number<'a, RefType> {
        type Family = NumberFamily;
        type RefType = RefType;
    }

    pub enum NumberContainer<'a> {
        Number(Number<'a, Storage>),
    }

    impl<'b> ContainerOf<Number<'b>> for NumberContainer<'b> {
        fn to_container(val: Number<'b>) -> Self {
            Self::Number(val)
        }
        fn from_container(&self) -> Result<&Number<'b>, graph::Error> {
            match self {
                Self::Number(val) => Ok(val),
            }
        }
    }
}

impl<'a, Container: graph2::ContainerOf<peano::Number<'a>> + 'a>
    graph2::TypedTree<peano::Number<'a>, Container>
{
    fn new(val: u8) -> Self {
        let mut builder = graph2::Builder::new();
        let mut a = builder.push(peano::Number::Zero);
        for _ in 0..val {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.into()
    }
}

impl<'a, Container: graph2::ContainerOf<peano::Number<'a>>>
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
        let mut builder = graph2::Builder::new::<peano::NumberContainer>();
        let mut a: graph2::BuilderRef<peano::Number<_>> =
            builder.push::<peano::Number<graph2::Builder>>(peano::Number::<graph2::Builder>::Zero);
        for _ in 0..3 {
            a = builder.push::<peano::Number<graph2::Builder>>(
                peano::Number::<graph2::Builder>::Successor(a),
            );
        }
        builder.into()
    };
}

#[test]
fn construct_unannotated() {
    let _three: graph2::TypedTree<peano::Number, peano::NumberContainer> = {
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
    let _three = graph2::TypedTree::<peano::Number, peano::NumberContainer>::new(3);
}

#[test]
fn match_root() -> Result<(), graph::Error> {
    let zero = graph2::TypedTree::<peano::Number, peano::NumberContainer>::new(0);
    let one = graph2::TypedTree::<peano::Number, peano::NumberContainer>::new(1);

    use peano::Number;
    assert!(matches!(zero.root().borrow()?, Number::Zero));
    assert!(matches!(one.root().borrow()?, Number::Successor(_)));

    Ok(())
}

#[test]
fn match_live_ref() -> Result<(), graph::Error> {
    let three = graph2::TypedTree::<peano::Number, peano::NumberContainer>::new(3);

    use peano::Number;
    let value = match three.root().borrow()? {
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
    let three = graph2::TypedTree::<peano::Number, peano::NumberContainer>::new(3);
    let value = three.root().borrow()?.value();
    assert_eq!(value, 3);
    Ok(())
}

pub mod direct_expr {
    // Initial definition
    // enum IntExpr<'a> {
    //     Int(i64),
    //     IntRef(&'a i64),
    //     Add(IntExpr, IntExpr),
    // }

    use super::graph2::*;

    pub enum IntExpr<'a, R: RecursiveRefType + 'a = Storage> {
        Int(R::Value<'a, i64>),
        IntRef(R::Value<'a, &'a i64>),
        Add(
            R::Ref<'a, IntExpr<'a, Storage>>,
            R::Ref<'a, IntExpr<'a, Storage>>,
        ),
    }

    pub struct IntExprFamily;

    impl RecursiveFamily for IntExprFamily {
        type Obj<'a, R: RecursiveRefType + 'a> = IntExpr<'a, R>;

        fn builder_to_storage<'a>(
            builder_obj: Self::Obj<'a, Builder>,
            converter: BuilderToStorage,
        ) -> Self::Obj<'a, Storage> {
            match builder_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => {
                    IntExpr::Add(converter.move_reference(a), converter.move_reference(b))
                }
            }
        }

        fn storage_to_visiting<'a, Container>(
            storage_obj: &'a Self::Obj<'a, Storage>,
            converter: StorageToVisiting<'a, Container>,
        ) -> Self::Obj<'a, Visiting<'a, Container>> {
            match storage_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => {
                    IntExpr::Add(converter.view_reference(a), converter.view_reference(b))
                }
            }
        }
    }

    impl<'a, RefType: RecursiveRefType> RecursiveObj<'a> for IntExpr<'a, RefType> {
        type Family = IntExprFamily;
        type RefType = RefType;
    }

    pub enum IntExprContainer<'a> {
        IntExpr(IntExpr<'a, Storage>),
    }

    impl<'b> ContainerOf<IntExpr<'b>> for IntExprContainer<'b> {
        fn to_container(val: IntExpr<'b>) -> Self {
            Self::IntExpr(val)
        }
        fn from_container(&self) -> Result<&IntExpr<'b>, graph::Error> {
            match self {
                Self::IntExpr(val) => Ok(val),
            }
        }
    }
}

impl<'a, Container: graph2::ContainerOf<direct_expr::IntExpr<'a>>>
    direct_expr::IntExpr<'a, graph2::Visiting<'a, Container>>
{
    fn eval(self) -> i64 {
        match self {
            Self::Int(val) => *val,
            Self::IntRef(val) => **val,
            Self::Add(a, b) => a.borrow().unwrap().eval() + b.borrow().unwrap().eval(),
        }
    }
}

#[test]
fn eval_int_expr() -> Result<(), graph::Error> {
    use direct_expr::IntExpr;
    let expr: graph2::TypedTree<IntExpr, direct_expr::IntExprContainer> = {
        let mut builder = graph2::Builder::new();
        let a = builder.push(IntExpr::Int(5));
        let b = builder.push(IntExpr::Int(10));
        let c = builder.push(IntExpr::Add(a, b));
        let d = builder.push(IntExpr::Int(100));
        builder.push(IntExpr::Add(c, d));
        builder.into()
    };

    let value: i64 = expr.root().borrow()?.eval();

    assert_eq!(value, 115);

    Ok(())
}

#[test]
fn eval_int_expr_with_reference() -> Result<(), graph::Error> {
    use direct_expr::IntExpr;

    let data = vec![5, 10, 100];

    let expr: graph2::TypedTree<IntExpr, direct_expr::IntExprContainer> = {
        let mut builder = graph2::Builder::new();
        let a = builder.push(IntExpr::IntRef(&data[0]));
        let b = builder.push(IntExpr::IntRef(&data[1]));
        let c = builder.push(IntExpr::Add(a, b));
        let d = builder.push(IntExpr::IntRef(&data[2]));
        builder.push(IntExpr::Add(c, d));
        builder.into()
    };

    let value: i64 = expr.root().borrow()?.eval();

    assert_eq!(value, 115);

    Ok(())
}
