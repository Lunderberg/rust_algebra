#![allow(dead_code)]

use graph2::TryAsRef;

mod graph2 {
    use std::fmt::{Display, Formatter};
    use std::marker::PhantomData;

    ///////////////////////////////////////////////////////////
    ////////////// Enable self-referential types //////////////
    ///////////////////////////////////////////////////////////

    /// Usage type (e.g. Storage, Builder, Visitor)
    pub trait RecursiveRefType<'a>: 'a {
        type Ref<'b, Family>
        where
            'a: 'b,
            Family: RecursiveFamily<'a>;
        type Value<'b, T: 'b>
        where
            'a: 'b;
    }

    /// Meta-object, at compile-time can generate an enum for a
    /// specific usage type.  At runtime, can convert between enums
    /// of different usage types.
    pub trait RecursiveFamily<'a>: 'a {
        type Builder;

        type Storage;

        type Visiting<'b, Container: 'a>
        where
            'a: 'b;

        fn builder_to_storage(builder_obj: Self::Builder, new_pos: usize) -> Self::Storage;

        fn storage_to_visiting<'b, Container: 'a>(
            storage_obj: &'b Self::Storage,
            view: &'b [Container],
        ) -> Self::Visiting<'b, Container>
        where
            'a: 'b;
    }

    pub trait TryAsRef<T> {
        type Error;
        fn try_as_ref(&self) -> Result<&T, Self::Error>;
    }

    pub trait BuilderObj<'a>: 'a {
        type Family: RecursiveFamily<'a, Builder = Self>;
    }

    pub trait StorageObj<'a>: 'a {
        type Family: RecursiveFamily<'a, Storage = Self>;
    }

    pub trait VisitingObj<'a: 'b, 'b, Container: 'a>: 'a {
        type Family: RecursiveFamily<'a, Visiting<'b, Container> = Self>;
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

    /////////////////////////////////////
    ////////////// Storage //////////////
    /////////////////////////////////////

    /// A usage annotation for objects that may be stored in the
    /// linearized structure.
    pub struct Storage;

    impl<'a> RecursiveRefType<'a> for Storage {
        type Ref<'b, Family: RecursiveFamily<'a>> = StorageRef<Family> where 'a:'b;
        type Value<'b, T: 'b> = T where 'a:'b;
    }

    /// Represents a reference in the linearized structure.
    pub struct StorageRef<Family> {
        /// Position of the referred-to type, relative to the position
        /// of the `RecursiveFamily::Obj<Storage>` that holds the
        /// `StorageRef`.
        rel_pos: usize,
        _node: PhantomData<*const Family>,
    }

    impl<'a, Family: RecursiveFamily<'a>> StorageRef<Family> {
        pub fn to_visiting<'b, Container: 'a>(
            &'b self,
            view: &'b [Container],
        ) -> VisitingRef<'a, 'b, Family, Container>
        where
            'a: 'b,
        {
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
    pub struct TypedTree<'a, Family: RecursiveFamily<'a>, Container: 'a> {
        nodes: Vec<Container>,
        _phantom: PhantomData<&'a Family>,
    }

    impl<'a, Family: RecursiveFamily<'a>, Container> TypedTree<'a, Family, Container> {
        /// Returns a reference to the root node of the tree
        pub fn root<'b>(&'b self) -> VisitingRef<'a, 'b, Family, Container>
        where
            'a: 'b,
        {
            VisitingRef {
                view: &self.nodes,
                _phantom: PhantomData,
            }
        }
    }

    impl<'a, Family: RecursiveFamily<'a>, Container: 'a> Display for TypedTree<'a, Family, Container>
    where
        for<'b> Family::Visiting<'b, Container>: Display,
        Container: TryAsRef<Family::Storage, Error = graph::Error>,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
            write!(f, "{}", self.root())
        }
    }

    /////////////////////////////////////
    ////////////// Builder //////////////
    /////////////////////////////////////

    pub struct BuilderRefType;

    /// A constructor used to generate a `TypedTree<Container>`.
    pub struct Builder<Container> {
        nodes: Vec<Container>,
    }

    impl<Container> Builder<Container> {
        /// Constructs an empty `Builder`.
        pub fn new() -> Self {
            Self { nodes: Vec::new() }
        }
    }

    /// Reference type used while building a tree.  Any time the user
    /// pushes a node into the builder, they receive a reference.
    /// That reference may then be used to construct additional
    /// builder nodes.
    pub struct BuilderRef<'a, Family: RecursiveFamily<'a>> {
        abs_pos: usize,
        _node: PhantomData<&'a Family>,
    }

    impl<'a> RecursiveRefType<'a> for BuilderRefType {
        type Ref<'b, Family: RecursiveFamily<'a>> = BuilderRef<'a, Family> where 'a:'b;
        type Value<'b, T: 'b> = T where 'a:'b;
    }

    impl<Container> Builder<Container> {
        /// Insert a new node to the builder
        pub fn push<'a, Family: RecursiveFamily<'a>>(
            &mut self,
            builder_obj: Family::Builder,
        ) -> BuilderRef<'a, Family>
        where
            Family::Storage: Into<Container>,
            Container: 'a,
        {
            let abs_pos = self.nodes.len();
            let storage_obj = Family::builder_to_storage(builder_obj, abs_pos);
            self.nodes.push(storage_obj.into());
            BuilderRef {
                abs_pos,
                _node: PhantomData,
            }
        }

        pub fn finalize<'a, Family: RecursiveFamily<'a>>(
            self,
            top_node: BuilderRef<'a, Family>,
        ) -> TypedTree<'a, Family, Container> {
            let mut nodes = self.nodes;

            // These should usually be no-ops, since passing something
            // other than the most recent reference would mean that
            // previously generated nodes have no effect.  However, in
            // that case, the truncate() call is necessary for correct
            // behavior.
            nodes.truncate(top_node.abs_pos + 1);
            nodes.shrink_to_fit();

            TypedTree {
                nodes,
                _phantom: PhantomData,
            }
        }
    }

    impl<'a, Family: RecursiveFamily<'a>> BuilderRef<'a, Family> {
        pub fn to_storage(&self, new_pos: usize) -> StorageRef<Family> {
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
        _phantom: PhantomData<&'a Container>,
    }

    pub struct VisitingRef<'a: 'b, 'b, Family: RecursiveFamily<'a>, Container> {
        view: &'b [Container],
        _phantom: PhantomData<&'a Family>,
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily<'a>, Container> Clone
        for VisitingRef<'a, 'b, Family, Container>
    {
        fn clone(&self) -> Self {
            VisitingRef {
                view: self.view,
                _phantom: PhantomData,
            }
        }
    }
    impl<'a: 'b, 'b, Family: RecursiveFamily<'a>, Container> Copy
        for VisitingRef<'a, 'b, Family, Container>
    {
    }

    impl<'a, Container> RecursiveRefType<'a> for Visiting<'a, Container> {
        type Ref<'b, Family: RecursiveFamily<'a>> = VisitingRef<'a, 'b, Family, Container> where 'a:'b;
        type Value<'b, T: 'b> = &'b T where 'a:'b;
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily<'a>, Container> VisitingRef<'a, 'b, Family, Container> {
        pub fn borrow(self) -> Result<Family::Visiting<'b, Container>, graph::Error>
        where
            Container: TryAsRef<Family::Storage, Error = graph::Error>,
        {
            let container: &Container = self.view.last().ok_or(graph::Error::EmptyExpression)?;
            let node: &Family::Storage = container.try_as_ref()?;
            let view = self.view;
            let live_ref = Family::storage_to_visiting(node, view);
            Ok(live_ref)
        }

        pub fn ref_equals(&self, other: Self) -> bool {
            self.view.as_ptr_range() == other.view.as_ptr_range()
        }

        pub fn structural_equals(&self, _other: Self) -> bool {
            todo!()
        }
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily<'a>, Container: 'a> Display
        for VisitingRef<'a, 'b, Family, Container>
    where
        Family::Visiting<'b, Container>: Display,
        Container: TryAsRef<Family::Storage, Error = graph::Error>,
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

    pub enum Number<'a: 'b, 'b, R: RecursiveRefType<'a> = Storage> {
        Zero,
        Successor(R::Ref<'b, NumberFamily>),
    }

    pub struct NumberFamily;

    impl<'a> RecursiveFamily<'a> for NumberFamily {
        type Builder = Number<'a, 'a, BuilderRefType>;

        type Storage = Number<'a, 'a, Storage>;

        type Visiting<'b, Container: 'a> = Number<'a, 'b, Visiting<'a, Container>> where 'a:'b;

        fn builder_to_storage(builder_obj: Self::Builder, new_pos: usize) -> Self::Storage {
            match builder_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'b, Container: 'a>(
            storage_obj: &'b Self::Storage,
            view: &'b [Container],
        ) -> Self::Visiting<'b, Container>
        where
            'a: 'b,
        {
            match storage_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_visiting(view)),
            }
        }
    }

    // impl<'a> BuilderObj<'a> for Number<'a, 'a, Builder<Container>> {
    //     type Family = NumberFamily;
    // }

    pub enum NumberContainer<'a> {
        Number(Number<'a, 'a, Storage>),
    }

    impl<'a> From<Number<'a, 'a, Storage>> for NumberContainer<'a> {
        fn from(val: Number<'a, 'a, Storage>) -> Self {
            NumberContainer::Number(val)
        }
    }

    impl<'a> TryAsRef<Number<'a, 'a, Storage>> for NumberContainer<'a> {
        type Error = graph::Error;
        fn try_as_ref(&self) -> Result<&Number<'a, 'a, Storage>, Self::Error> {
            match self {
                NumberContainer::Number(val) => Ok(val),
            }
        }
    }

    impl<'a> ContainerOf<Number<'a, 'a, Storage>> for NumberContainer<'a> {
        fn to_container(val: Number<'a, 'a, Storage>) -> Self {
            NumberContainer::Number(val)
        }
        fn from_container(&self) -> Result<&Number<'a, 'a, Storage>, graph::Error> {
            match self {
                NumberContainer::Number(val) => Ok(val),
            }
        }
    }
}

impl<'a, Container: From<peano::Number<'a, 'a>>>
    graph2::TypedTree<'a, peano::NumberFamily, Container>
{
    fn new(val: u8) -> Self {
        let mut builder = graph2::Builder::new();
        let mut a = builder.push(peano::Number::Zero);
        for _ in 0..val {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.finalize(a)
    }
}

impl<'a: 'b, 'b, Container: TryAsRef<peano::Number<'a, 'a>, Error = graph::Error>>
    peano::Number<'a, 'b, graph2::Visiting<'a, Container>>
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
    let _three: graph2::TypedTree<peano::NumberFamily, peano::NumberContainer> = {
        let mut builder = graph2::Builder::<peano::NumberContainer>::new();
        let mut a: graph2::BuilderRef<peano::NumberFamily> =
            builder.push::<peano::NumberFamily>(peano::Number::<graph2::BuilderRefType>::Zero);
        for _ in 0..3 {
            a = builder
                .push::<peano::NumberFamily>(peano::Number::<graph2::BuilderRefType>::Successor(a));
        }
        builder.finalize(a)
    };
}

#[test]
fn construct_unannotated() {
    let _three: graph2::TypedTree<peano::NumberFamily, peano::NumberContainer> = {
        let mut builder = graph2::Builder::new();
        let mut a = builder.push(peano::Number::Zero);

        for _ in 0..3 {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.finalize(a)
    };
}

#[test]
fn call_static_method() {
    let _three = graph2::TypedTree::<peano::NumberFamily, peano::NumberContainer>::new(3);
}

#[test]
fn match_root() -> Result<(), graph::Error> {
    let zero = graph2::TypedTree::<peano::NumberFamily, peano::NumberContainer>::new(0);
    let one = graph2::TypedTree::<peano::NumberFamily, peano::NumberContainer>::new(1);

    use peano::Number;
    assert!(matches!(zero.root().borrow()?, Number::Zero));
    assert!(matches!(one.root().borrow()?, Number::Successor(_)));

    Ok(())
}

#[test]
fn match_live_ref() -> Result<(), graph::Error> {
    let three = graph2::TypedTree::<peano::NumberFamily, peano::NumberContainer>::new(3);

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
    let three = graph2::TypedTree::<peano::NumberFamily, peano::NumberContainer>::new(3);
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

    pub enum IntExpr<'a: 'b, 'b, R: RecursiveRefType<'a> = Storage> {
        Int(R::Value<'b, i64>),
        IntRef(R::Value<'b, &'b i64>),
        Add(R::Ref<'b, IntExprFamily>, R::Ref<'b, IntExprFamily>),
    }

    pub struct IntExprFamily;

    impl<'a> RecursiveFamily<'a> for IntExprFamily {
        type Builder = IntExpr<'a, 'a, BuilderRefType>;

        type Storage = IntExpr<'a, 'a, Storage>;

        type Visiting<'b, Container:'a> = IntExpr<'a, 'b, Visiting<'a, Container>> where 'a:'b;

        fn builder_to_storage(builder_obj: Self::Builder, new_pos: usize) -> Self::Storage {
            match builder_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_storage(new_pos), b.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'b, Container: 'a>(
            storage_obj: &'b Self::Storage,
            view: &'b [Container],
        ) -> Self::Visiting<'b, Container>
        where
            'a: 'b,
        {
            match storage_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_visiting(view), b.to_visiting(view)),
            }
        }
    }

    pub enum IntExprContainer<'a> {
        IntExpr(IntExpr<'a, 'a, Storage>),
    }

    impl<'a> From<IntExpr<'a, 'a, Storage>> for IntExprContainer<'a> {
        fn from(val: IntExpr<'a, 'a, Storage>) -> Self {
            IntExprContainer::IntExpr(val)
        }
    }

    impl<'a> TryAsRef<IntExpr<'a, 'a, Storage>> for IntExprContainer<'a> {
        type Error = graph::Error;
        fn try_as_ref(&self) -> Result<&IntExpr<'a, 'a, Storage>, Self::Error> {
            match self {
                IntExprContainer::IntExpr(val) => Ok(val),
            }
        }
    }

    impl<'a> ContainerOf<IntExpr<'a, 'a, Storage>> for IntExprContainer<'a> {
        fn to_container(val: IntExpr<'a, 'a, Storage>) -> Self {
            IntExprContainer::IntExpr(val)
        }
        fn from_container(&self) -> Result<&IntExpr<'a, 'a, Storage>, graph::Error> {
            match self {
                IntExprContainer::IntExpr(val) => Ok(val),
            }
        }
    }

    impl<'a: 'b, 'b, Container: TryAsRef<IntExpr<'a, 'a>, Error = graph::Error>> Display
        for IntExpr<'a, 'b, Visiting<'a, Container>>
    {
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

impl<'a: 'b, 'b, Container: TryAsRef<direct_expr::IntExpr<'a, 'a>, Error = graph::Error>>
    direct_expr::IntExpr<'a, 'b, graph2::Visiting<'a, Container>>
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
    use direct_expr::{IntExpr, IntExprContainer, IntExprFamily};
    let expr: graph2::TypedTree<IntExprFamily, IntExprContainer> = {
        let mut builder = graph2::Builder::new();
        let a = builder.push(IntExpr::Int(5));
        let b = builder.push(IntExpr::Int(10));
        let c = builder.push(IntExpr::Add(a, b));
        let d = builder.push(IntExpr::Int(100));
        let e = builder.push(IntExpr::Add(c, d));
        builder.finalize(e)
    };

    let value: i64 = expr.root().borrow()?.eval();

    assert_eq!(value, 115);

    Ok(())
}

#[test]
fn eval_int_expr_with_reference() -> Result<(), graph::Error> {
    use direct_expr::{IntExpr, IntExprContainer, IntExprFamily};

    let data = vec![5, 10, 100];

    let expr: graph2::TypedTree<IntExprFamily, IntExprContainer> = {
        let mut builder = graph2::Builder::new();
        let a = builder.push(IntExpr::IntRef(&data[0]));
        let b = builder.push(IntExpr::IntRef(&data[1]));
        let c = builder.push(IntExpr::Add(a, b));
        let d = builder.push(IntExpr::IntRef(&data[2]));
        let e = builder.push(IntExpr::Add(c, d));
        builder.finalize(e)
    };

    let value: i64 = expr.root().borrow()?.eval();

    assert_eq!(value, 115);

    Ok(())
}

#[test]
fn int_self_ref_equal() {
    use direct_expr::{IntExpr, IntExprContainer, IntExprFamily};
    use graph2::{Builder, TypedTree};

    let expr1: TypedTree<IntExprFamily, IntExprContainer> = {
        let mut builder = Builder::new();
        let a = builder.push(IntExpr::Int(5));
        builder.finalize(a)
    };
    assert!(expr1.root().ref_equals(expr1.root()));
}

#[test]
fn int_equivalent_tree_not_ref_equal() {
    use direct_expr::{IntExpr, IntExprContainer, IntExprFamily};
    use graph2::{Builder, TypedTree};

    let expr1: TypedTree<IntExprFamily, IntExprContainer> = {
        let mut builder = Builder::new();
        let a = builder.push(IntExpr::Int(5));
        builder.finalize(a)
    };
    let expr2: TypedTree<IntExprFamily, IntExprContainer> = {
        let mut builder = Builder::new();
        let a = builder.push(IntExpr::Int(5));
        builder.finalize(a)
    };
    assert!(!expr1.root().ref_equals(expr2.root()));
}

// #[test]
// fn int_equivalent_tree_structurally_equal() {
//     use direct_expr::{IntExprFamily, IntExpr};
//     use graph2::{Builder, TypedTree};

//     let expr1: TypedTree<IntExprFamily> = {
//         let mut builder = Builder::new();
//         let a = builder.push(IntExpr::Int(5));
//         builder.finalize(a)
//     };
//     let expr2: TypedTree<IntExprFamily> = {
//         let mut builder = Builder::new();
//         let a = builder.push(IntExpr::Int(5));
//         builder.finalize(a)
//     };
//     assert!(expr1.root().structural_equals(expr2.root()));
// }

#[test]
fn int_equivalent_tree_structurally_equal() {
    use direct_expr::{IntExpr, IntExprContainer, IntExprFamily};
    use graph2::{Builder, TypedTree};

    let expr: TypedTree<IntExprFamily, IntExprContainer> = {
        let mut builder = Builder::new();
        let a = builder.push(IntExpr::Int(5));
        let b = builder.push(IntExpr::Int(10));
        let c = builder.push(IntExpr::Add(a, b));
        builder.finalize(c)
    };

    assert_eq!(format!("{}", expr.root().borrow().unwrap()), "5 + 10");
    assert_eq!(format!("{}", expr.root()), "5 + 10");
    assert_eq!(format!("{expr}"), "5 + 10");
}
