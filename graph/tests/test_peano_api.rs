#![allow(dead_code)]

mod graph2 {
    use std::fmt::{Display, Formatter};
    use std::marker::PhantomData;

    ///////////////////////////////////////////////////////////
    ////////////// Enable self-referential types //////////////
    ///////////////////////////////////////////////////////////

    /// Usage type (e.g. Storage, Builder, Visitor)
    pub trait RecursiveRefType {
        type Ref<'a, T: 'a>;
        type Value<'a, T: 'a>;
    }

    /// Meta-object, at compile-time can generate an enum for a
    /// specific usage type.  At runtime, can convert between enums
    /// of different usage types.
    pub trait RecursiveFamily {
        type Obj<'a, R: RecursiveRefType + 'a>;

        fn builder_to_storage<'a>(
            builder_obj: Self::Obj<'a, Builder>,
            new_pos: usize,
        ) -> Self::Obj<'a, Storage>;

        fn storage_to_visiting<'a, Container>(
            storage_obj: &'a Self::Obj<'a, Storage>,
            view: &'a [Container],
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
        type Ref<'a, T: 'a> = StorageRef<T>;
        type Value<'a, T: 'a> = T;
    }

    /// Represents a reference in the linearized structure.
    pub struct StorageRef<T> {
        /// Position of the referred-to type, relative to the position
        /// of the `RecursiveFamily::Obj<Storage>` that holds the
        /// `StorageRef`.
        rel_pos: usize,
        _node: PhantomData<*const T>,
    }

    impl<T> StorageRef<T> {
        pub fn to_visiting<'a, Container>(
            &'a self,
            view: &'a [Container],
        ) -> VisitingRef<'a, T, Container> {
            let index = view.len().checked_sub(self.rel_pos).unwrap_or(0);
            VisitingRef {
                view: &view[..index],
                _phantom: PhantomData,
            }
        }
    }

    ///////////////////////////////////////
    ////////////// TypedTree //////////////
    ///////////////////////////////////////

    /// A container for an entire tree structure.  The container must
    /// be able to represent any individual node type that may occur
    /// within the tree.  These should be expected by implementing
    /// `ContainerOf<Node>` for each type that may be contained.
    pub struct TypedTree<
        RootNodeType: HasDefaultContainer,
        Container = <RootNodeType as HasDefaultContainer>::DefaultContainer,
    > {
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

    /// Utility trait for providing a default container.
    ///
    /// Most of the time, a tree should be contained in the enum that
    /// was generated to contain it, or any recursively referenced
    /// type contained by it.  However, in some cases it may be
    /// desirable to use a broader container type.  For example,
    /// consider the following structure.
    ///
    /// ```
    /// #[recursive_graph]
    /// mod my_graph {
    ///     enum BoolExpr {
    ///         And(BoolExpr, BoolExpr),
    ///         Equal(IntExpr, IntExpr),
    ///     }
    ///
    ///     enum IntExpr {
    ///         Int(i64),
    ///         Add(IntExpr, IntExpr),
    ///     }
    /// }
    /// ```
    ///
    /// An `IntExpr` can only refer to itself, while a `BoolExpr` may
    /// refer either to itself or to an `IntExpr`.  The auto-generated
    /// type `my_graph::container::IntExpr` and
    /// `my_graph::container::BoolExpr` may be used for their
    /// respective types.  However, an `IntExpr` stored inside a
    /// `Vec<container::IntExpr>` would need to be converted into a
    /// `Vec<container::BoolExpr>` prior to use in a `BoolExpr`.  The
    /// user may want to use the `IntExpr` inside a
    /// `Vec<container::BoolExpr>` from the start, such that no
    /// conversion is required.
    ///
    /// Since this isn't the typical case, this trait is implemented
    /// for all recursive types, allowing a default container to be
    /// chosen for each recursive type.
    pub trait HasDefaultContainer: Sized {
        type DefaultContainer: ContainerOf<Self>;
    }

    impl<RootNodeType: HasDefaultContainer, Container> TypedTree<RootNodeType, Container> {
        /// Returns a reference to the root node of the tree
        pub fn root<'a: 'b, 'b>(&'a self) -> VisitingRef<'b, RootNodeType, Container> {
            VisitingRef {
                view: &self.nodes,
                _phantom: PhantomData,
            }
        }
    }

    // impl<'a: 'b, 'b, RootNodeType: HasDefaultContainer, Container: 'a> Display
    //     for TypedTree<RootNodeType, Container>
    // where
    //     VisitingRef<'b, RootNodeType, Container>: Display,
    //     Self: 'b,
    // {
    //     fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
    //         write!(f, "{}", self.root())
    //     }
    // }

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
    pub struct BuilderRef<T> {
        abs_pos: usize,
        _node: PhantomData<*const T>,
    }

    impl RecursiveRefType for Builder {
        type Ref<'a, T: 'a> = BuilderRef<T>;
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
            let storage_obj =
                <T::Family as RecursiveFamily>::builder_to_storage(builder_obj, abs_pos);
            let container = Container::to_container(storage_obj);
            self.nodes.push(container);
            BuilderRef {
                abs_pos,
                _node: PhantomData,
            }
        }
    }

    impl<RootNodeType: HasDefaultContainer, Container> From<BuilderObj<Container>>
        for TypedTree<RootNodeType, Container>
    {
        fn from(builder: BuilderObj<Container>) -> Self {
            Self {
                nodes: builder.nodes,
                _phantom: PhantomData,
            }
        }
    }

    impl<T> BuilderRef<T> {
        pub fn to_storage(&self, new_pos: usize) -> StorageRef<T> {
            let rel_pos = new_pos
                .checked_sub(self.abs_pos)
                .expect("Invalid reference type");
            StorageRef {
                rel_pos,
                _node: PhantomData,
            }
        }
    }

    //////////////////////////////////////////
    //////////////   Visiting   //////////////
    //////////////////////////////////////////

    pub struct Visiting<'a, Container> {
        _a: PhantomData<&'a [Container]>,
    }

    pub struct VisitingRef<'a, T, Container> {
        view: &'a [Container],
        _phantom: PhantomData<*const T>,
    }

    impl<'a, T, Container> Clone for VisitingRef<'a, T, Container> {
        fn clone(&self) -> Self {
            VisitingRef {
                view: self.view,
                _phantom: self._phantom,
            }
        }
    }

    impl<'a, T, Container> Copy for VisitingRef<'a, T, Container> {}

    impl<'b, Container> RecursiveRefType for Visiting<'b, Container> {
        type Ref<'a, T: 'a> = VisitingRef<'b, T, Container>;
        type Value<'a, T: 'a> = &'a T;
    }

    impl<'a, NodeType, Container> VisitingRef<'a, NodeType, Container> {
        pub fn borrow(
            self,
        ) -> Result<
            <NodeType::Family as RecursiveFamily>::Obj<'a, Visiting<'a, Container>>,
            graph::Error,
        >
        where
            NodeType: RecursiveObj<'a, RefType = Storage>,
            Container: ContainerOf<NodeType>,
        {
            let container: &Container = self.view.last().ok_or(graph::Error::EmptyExpression)?;
            let node: &NodeType = container.from_container()?;
            let view = self.view;
            let live_ref = <NodeType::Family as RecursiveFamily>::storage_to_visiting(node, view);
            Ok(live_ref)
        }

        pub fn ref_equals<OtherContainer>(
            &self,
            other: VisitingRef<'a, NodeType, OtherContainer>,
        ) -> bool {
            fn pointer_region<N, C>(
                visiting_ref: &VisitingRef<'_, N, C>,
            ) -> std::ops::Range<usize> {
                let ptr_range = visiting_ref.view.as_ptr_range();
                let start = ptr_range.start as usize;
                let end = ptr_range.end as usize;
                start..end
            }
            pointer_region(self) == pointer_region(&other)
        }

        pub fn structural_equals<OtherContainer>(
            &self,
            _other: VisitingRef<'a, NodeType, OtherContainer>,
        ) -> bool {
            todo!()
        }
    }

    impl<'a, T, Container> Display for VisitingRef<'a, T, Container>
    where
        T: RecursiveObj<'a, RefType = Storage>,
        Container: ContainerOf<T>,
        <T::Family as RecursiveFamily>::Obj<'a, Visiting<'a, Container>>: Display,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
            write!(f, "{}", self.borrow().unwrap())
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

    pub enum Number<'a, R: RecursiveRefType + 'a = Storage> {
        Zero,
        Successor(R::Ref<'a, Number<'a>>),
    }

    pub struct NumberFamily;

    impl RecursiveFamily for NumberFamily {
        type Obj<'a, R: RecursiveRefType + 'a> = Number<'a, R>;

        fn builder_to_storage<'a>(
            builder_obj: Self::Obj<'a, Builder>,
            new_pos: usize,
        ) -> Self::Obj<'a, Storage> {
            match builder_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'a, Container>(
            storage_obj: &'a Self::Obj<'a, Storage>,
            view: &'a [Container],
        ) -> Self::Obj<'a, Visiting<'a, Container>> {
            match &storage_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_visiting(view)),
            }
        }
    }

    impl<'a, RefType: RecursiveRefType> RecursiveObj<'a> for Number<'a, RefType> {
        type Family = NumberFamily;
        type RefType = RefType;
    }

    pub enum NumberContainer<'a> {
        Number(Number<'a>),
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

    impl<'a> HasDefaultContainer for Number<'a> {
        type DefaultContainer = NumberContainer<'a>;
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
    assert!(matches!(zero.root().borrow()?, Number::Zero));
    assert!(matches!(one.root().borrow()?, Number::Successor(_)));

    Ok(())
}

#[test]
fn match_live_ref() -> Result<(), graph::Error> {
    let three = graph2::TypedTree::<peano::Number>::new(3);

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
    let three = graph2::TypedTree::<peano::Number>::new(3);
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
    use std::fmt::{Display, Formatter};

    pub enum IntExpr<'a, R: RecursiveRefType + 'a = Storage> {
        Int(R::Value<'a, i64>),
        IntRef(R::Value<'a, &'a i64>),
        Add(R::Ref<'a, IntExpr<'a>>, R::Ref<'a, IntExpr<'a>>),
    }

    pub struct IntExprFamily;

    impl RecursiveFamily for IntExprFamily {
        type Obj<'a, R: RecursiveRefType + 'a> = IntExpr<'a, R>;

        fn builder_to_storage<'a>(
            builder_obj: Self::Obj<'a, Builder>,
            new_pos: usize,
        ) -> Self::Obj<'a, Storage> {
            match builder_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_storage(new_pos), b.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'a, Container>(
            storage_obj: &'a Self::Obj<'a, Storage>,
            view: &'a [Container],
        ) -> Self::Obj<'a, Visiting<'a, Container>> {
            match storage_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_visiting(view), b.to_visiting(view)),
            }
        }
    }

    impl<'a, RefType: RecursiveRefType> RecursiveObj<'a> for IntExpr<'a, RefType> {
        type Family = IntExprFamily;
        type RefType = RefType;
    }

    pub enum IntExprContainer<'a> {
        IntExpr(IntExpr<'a>),
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

    impl<'a> HasDefaultContainer for IntExpr<'a> {
        type DefaultContainer = IntExprContainer<'a>;
    }

    impl<'a, Container: ContainerOf<IntExpr<'a>>> Display for IntExpr<'a, Visiting<'a, Container>> {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                IntExpr::Int(val) => write!(f, "{val}"),
                IntExpr::IntRef(external_val) => write!(f, "{external_val}"),
                IntExpr::Add(a, b) => {
                    write!(f, "{} + {}", a.borrow().unwrap(), b.borrow().unwrap())
                }
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
    let expr: graph2::TypedTree<IntExpr> = {
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

    let expr: graph2::TypedTree<IntExpr> = {
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

#[test]
fn int_self_ref_equal() {
    use direct_expr::IntExpr;
    use graph2::{Builder, TypedTree};

    let expr1: TypedTree<IntExpr> = {
        let mut builder = Builder::new();
        builder.push(IntExpr::Int(5));
        builder.into()
    };
    assert!(expr1.root().ref_equals(expr1.root()));
}

#[test]
fn int_equivalent_tree_not_ref_equal() {
    use direct_expr::IntExpr;
    use graph2::{Builder, TypedTree};

    let expr1: TypedTree<IntExpr> = {
        let mut builder = Builder::new();
        builder.push(IntExpr::Int(5));
        builder.into()
    };
    let expr2: TypedTree<IntExpr> = {
        let mut builder = Builder::new();
        builder.push(IntExpr::Int(5));
        builder.into()
    };
    assert!(!expr1.root().ref_equals(expr2.root()));
}

// #[test]
// fn int_equivalent_tree_structurally_equal() {
//     use direct_expr::IntExpr;
//     use graph2::{Builder, TypedTree};

//     let expr1: TypedTree<IntExpr> = {
//         let mut builder = Builder::new();
//         builder.push(IntExpr::Int(5));
//         builder.into()
//     };
//     let expr2: TypedTree<IntExpr> = {
//         let mut builder = Builder::new();
//         builder.push(IntExpr::Int(5));
//         builder.into()
//     };
//     assert!(expr1.root().structural_equals(expr2.root()));
// }

#[test]
fn int_equivalent_tree_structurally_equal() {
    use direct_expr::IntExpr;
    use graph2::{Builder, TypedTree};

    let expr: TypedTree<IntExpr> = {
        let mut builder = Builder::new();
        let a = builder.push(IntExpr::Int(5));
        let b = builder.push(IntExpr::Int(10));
        builder.push(IntExpr::Add(a, b));
        builder.into()
    };

    assert_eq!(format!("{}", expr.root().borrow().unwrap()), "5 + 10");
    assert_eq!(format!("{}", expr.root()), "5 + 10");
    //assert_eq!(format!("{expr}"), "5 + 10");
}
