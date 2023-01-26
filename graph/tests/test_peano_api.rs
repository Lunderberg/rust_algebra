#![allow(dead_code)]

/// Testing ground for API changes, before implementing all the
/// derivation macros.
///
/// - `implied_bounds`: https://github.com/rust-lang/rust/issues/44491
///
///   May improve all the `Container` bounds floating around.
///
/// - `trait_alias`: https://github.com/rust-lang/rust/issues/41517
///
///   May be used similar to `implied_bounds`, since the trait aliases
///   include any bounds on the generics they use.
///
/// - `provide_any`: https://github.com/rust-lang/rust/issues/96024
///
///   May replace the `TryAsRef` trait.  Depends on whether it
///   supports non-static lifetimes.
use graph2::{TryAsRef, TypedTree};

mod graph2 {
    use std::fmt::{Display, Formatter};
    use std::marker::PhantomData;

    ///////////////////////////////////////////////////////////
    ////////////// Enable self-referential types //////////////
    ///////////////////////////////////////////////////////////

    /// Usage type (e.g. Storage, Builder, Visitor)
    pub trait RecursiveRefType {
        type Ref<'a, Family>
        where
            Self: 'a;
        type Value<'a, T>
        where
            T: 'a;
    }

    pub trait RecursiveFamilyGAT {
        type Builder<'a>;

        type Storage<'a>;

        type Container<'a>;

        type Visiting<'a, Container>
        where
            Container: 'a;
    }

    /// Meta-object, at compile-time can generate an enum for a
    /// specific usage type.  At runtime, can convert between enums
    /// of different usage types.
    pub trait RecursiveFamily: RecursiveFamilyGAT {
        fn builder_to_storage<'a>(
            builder_obj: Self::Builder<'a>,
            new_pos: usize,
        ) -> Self::Storage<'a>;

        fn storage_to_visiting<'a, 'b, Container>(
            storage_obj: &'b Self::Storage<'a>,
            view: &'b [Container],
        ) -> Self::Visiting<'b, Container>
        where
            Container: 'b;
    }

    pub trait TryAsRef<T> {
        type Error: std::fmt::Debug;
        fn try_as_ref(&self) -> Result<&T, Self::Error>;
    }

    pub trait BuilderObj<'a>: 'a {
        type Family: RecursiveFamily<Builder<'a> = Self>;
    }

    /////////////////////////////////////
    ////////////// Storage //////////////
    /////////////////////////////////////

    /// A usage annotation for objects that may be stored in the
    /// linearized structure.
    pub struct Storage;

    impl RecursiveRefType for Storage {
        type Ref<'a, Family> = StorageRef<Family>
        where
            Self: 'a;
        type Value<'a, T> = T
        where
            T: 'a;
    }

    /// Represents a reference in the linearized structure.
    pub struct StorageRef<Family> {
        /// Position of the referred-to type, relative to the position
        /// of the `RecursiveFamily::Obj<Storage>` that holds the
        /// `StorageRef`.
        rel_pos: usize,
        _node: PhantomData<*const Family>,
    }

    impl<Family> StorageRef<Family> {
        pub fn to_visiting<'a, 'b, Container>(
            &'b self,
            view: &'b [Container],
        ) -> VisitingRef<'b, Family, Container> {
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
        'a,
        Family: RecursiveFamily,
        Container: 'a = <Family as RecursiveFamilyGAT>::Container<'a>,
    > {
        nodes: Vec<Container>,
        _phantom: PhantomData<&'a Family>,
    }

    impl<'a, Family: RecursiveFamily, Container> TypedTree<'a, Family, Container> {
        pub fn build<Func>(func: Func) -> Self
        where
            Family: RecursiveFamily<Container<'a> = Container>,
            Func: FnOnce(&mut Builder<Container>) -> BuilderRef<Family>,
        {
            Self::build_with_container(func)
        }

        pub fn build_with_container<Func>(func: Func) -> Self
        where
            Func: FnOnce(&mut Builder<Container>) -> BuilderRef<Family>,
        {
            let mut builder = Builder::new();
            let top_node = func(&mut builder);
            builder.finalize(top_node)
        }

        /// Returns a reference to the root node of the tree
        pub fn root(&self) -> VisitingRef<Family, Container> {
            VisitingRef {
                view: &self.nodes,
                _phantom: PhantomData,
            }
        }
    }

    impl<'a, Family: RecursiveFamily + 'a, Container: 'a> Display for TypedTree<'a, Family, Container>
    where
        for<'b> Family::Visiting<'b, Container>: Display,
        Container: TryAsRef<Family::Storage<'a>>,
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
    pub struct BuilderRef<Family> {
        abs_pos: usize,
        _node: PhantomData<*const Family>,
    }

    impl RecursiveRefType for BuilderRefType {
        type Ref<'a, Family> = BuilderRef<Family>
        where
            Self: 'a;
        type Value<'a, T> = T
        where
            T: 'a;
    }

    impl<Container> Builder<Container> {
        /// Insert a new node to the builder
        pub fn push<'a, Obj: BuilderObj<'a>>(&mut self, builder_obj: Obj) -> BuilderRef<Obj::Family>
        where
            <Obj::Family as RecursiveFamilyGAT>::Storage<'a>: Into<Container>,
            Container: 'a,
        {
            let abs_pos = self.nodes.len();
            let storage_obj = Obj::Family::builder_to_storage(builder_obj, abs_pos);
            self.nodes.push(storage_obj.into());
            BuilderRef {
                abs_pos,
                _node: PhantomData,
            }
        }

        pub fn finalize<'a, Family: RecursiveFamily + 'a>(
            self,
            top_node: BuilderRef<Family>,
        ) -> TypedTree<'a, Family, Container>
        where
            Container: 'a,
        {
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

    impl<Family> BuilderRef<Family> {
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

    pub struct Visiting<Container> {
        _phantom: PhantomData<*const Container>,
    }

    pub struct VisitingRef<'a, Family, Container> {
        view: &'a [Container],
        _phantom: PhantomData<*const Family>,
    }

    impl<'a, Family, Container> Clone for VisitingRef<'a, Family, Container> {
        fn clone(&self) -> Self {
            VisitingRef {
                view: self.view,
                _phantom: PhantomData,
            }
        }
    }
    impl<'a, Family, Container> Copy for VisitingRef<'a, Family, Container> {}

    impl<Container> RecursiveRefType for Visiting<Container> {
        type Ref<'a, Family> = VisitingRef<'a, Family, Container>
        where
            Self: 'a;
        type Value<'a, T> = &'a T
        where
            T: 'a;
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily + 'a, Container: 'a> VisitingRef<'b, Family, Container> {
        // TODO: Maybe a utility trait that is implemented whenever
        // there are TryAsRef implementations for all reachable
        // recursive types?  Repeating `where Container:
        // TryAsRef<...>` for all types is rather tedious.
        pub fn borrow(self) -> Family::Visiting<'b, Container>
        where
            Container: TryAsRef<Family::Storage<'a>>,
        {
            let container: &Container = self
                .view
                .last()
                .ok_or(graph::Error::EmptyExpression)
                .expect("Malformed tree found with empty node");
            let node: &Family::Storage<'a> = container
                .try_as_ref()
                .expect("Incorrect type found when visiting tree");
            let view = self.view;
            let live_ref = Family::storage_to_visiting(node, view);
            live_ref
        }

        pub fn ref_equals(&self, other: Self) -> bool {
            self.view.as_ptr_range() == other.view.as_ptr_range()
        }
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily + 'a, Container: 'a> std::cmp::PartialEq
        for VisitingRef<'b, Family, Container>
    where
        Container: TryAsRef<Family::Storage<'a>>,
        Family::Visiting<'b, Container>: PartialEq,
    {
        fn eq(&self, rhs: &Self) -> bool {
            self.ref_equals(*rhs) || (self.borrow() == rhs.borrow())
        }
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily + 'a, Container: 'a> std::hash::Hash
        for VisitingRef<'b, Family, Container>
    where
        Container: TryAsRef<Family::Storage<'a>>,
        Family::Visiting<'b, Container>: std::hash::Hash,
    {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.borrow().hash(state)
        }
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily + 'a, Container: 'a> Display
        for VisitingRef<'b, Family, Container>
    where
        Family::Visiting<'b, Container>: Display,
        Container: TryAsRef<Family::Storage<'a>>,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
            write!(f, "{}", self.borrow())
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
        Successor(R::Ref<'a, NumberFamily>),
    }

    pub struct NumberFamily;

    impl RecursiveFamilyGAT for NumberFamily {
        type Builder<'a> = Number<'a, BuilderRefType>;

        type Storage<'a> = Number<'a, Storage>;

        type Container<'a> = NumberContainer<'a>;

        type Visiting<'a, Container> = Number<'a, Visiting<Container>>
        where
            Container: 'a;
    }

    impl RecursiveFamily for NumberFamily {
        fn builder_to_storage<'a>(
            builder_obj: Self::Builder<'a>,
            new_pos: usize,
        ) -> Self::Storage<'a> {
            match builder_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'a, 'b, Container>(
            storage_obj: &'b Self::Storage<'a>,
            view: &'b [Container],
        ) -> Self::Visiting<'b, Container>
        where
            Container: 'b,
        {
            match storage_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_visiting(view)),
            }
        }
    }

    impl<'a> BuilderObj<'a> for Number<'a, BuilderRefType> {
        type Family = NumberFamily;
    }

    pub enum NumberContainer<'a> {
        Number(Number<'a, Storage>),
    }

    impl<'a> From<Number<'a, Storage>> for NumberContainer<'a> {
        fn from(val: Number<'a, Storage>) -> Self {
            NumberContainer::Number(val)
        }
    }

    impl<'a> TryAsRef<Number<'a, Storage>> for NumberContainer<'a> {
        type Error = graph::Error;
        fn try_as_ref(&self) -> Result<&Number<'a, Storage>, Self::Error> {
            match self {
                NumberContainer::Number(val) => Ok(val),
            }
        }
    }

    impl<'a: 'b, 'b, Container: 'a + TryAsRef<Number<'a>>> std::cmp::PartialEq
        for Number<'b, Visiting<Container>>
    {
        fn eq(&self, rhs: &Self) -> bool {
            match (self, rhs) {
                (Number::Zero, Number::Zero) => true,
                (Number::Successor(a), Number::Successor(b)) => a == b,
                _ => false,
            }
        }
    }

    impl<'a: 'b, 'b, Container: 'a + TryAsRef<Number<'a>>> std::hash::Hash
        for Number<'b, Visiting<Container>>
    {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            std::mem::discriminant(self).hash(state);
            match self {
                Number::Zero => {}
                Number::Successor(prev) => prev.hash(state),
            }
        }
    }
}

impl<'a, Container: From<peano::Number<'a>>> graph2::TypedTree<'a, peano::NumberFamily, Container> {
    fn new(val: u8) -> Self {
        let mut builder = graph2::Builder::new();
        let mut a = builder.push(peano::Number::Zero);
        for _ in 0..val {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.finalize(a)
    }
}

impl<'a: 'b, 'b, Container: 'a + TryAsRef<peano::Number<'a>>>
    peano::Number<'b, graph2::Visiting<Container>>
{
    fn value(&self) -> usize {
        match self {
            peano::Number::Zero => 0,
            peano::Number::Successor(live_ref) => {
                let num = live_ref.borrow();
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
            builder.push::<peano::Number<graph2::BuilderRefType>>(
                peano::Number::<graph2::BuilderRefType>::Zero,
            );
        for _ in 0..3 {
            a = builder.push::<peano::Number<graph2::BuilderRefType>>(peano::Number::<
                graph2::BuilderRefType,
            >::Successor(a));
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
    assert!(matches!(zero.root().borrow(), Number::Zero));
    assert!(matches!(one.root().borrow(), Number::Successor(_)));

    Ok(())
}

#[test]
fn match_live_ref() -> Result<(), graph::Error> {
    let three = graph2::TypedTree::<peano::NumberFamily, peano::NumberContainer>::new(3);

    use peano::Number;
    let value = match three.root().borrow() {
        Number::Zero => 0,
        Number::Successor(a) => match a.borrow() {
            Number::Zero => 1,
            Number::Successor(b) => match b.borrow() {
                Number::Zero => 2,
                Number::Successor(c) => match c.borrow() {
                    Number::Zero => 3,
                    Number::Successor(d) => match d.borrow() {
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
    let value = three.root().borrow().value();
    assert_eq!(value, 3);
    Ok(())
}

pub mod expr {
    // Initial definition
    // mod expr {
    //     enum IntExpr<'a> {
    //         Int(i64),
    //         IntRef(&'a i64),
    //         Add(IntExpr, IntExpr),
    //         Floor(FloatExpr),
    //     }
    //     enum FloatExpr {
    //         Float(f64),
    //         Add(FloatExpr,FloatExpr),
    //         Cast(IntExpr),
    //     }
    // }

    use super::graph2::*;
    use std::fmt::{Display, Formatter};

    pub enum IntExpr<'a, R: RecursiveRefType + 'a = Storage> {
        Int(R::Value<'a, i64>),
        IntRef(R::Value<'a, &'a i64>),
        Add(R::Ref<'a, IntExprFamily>, R::Ref<'a, IntExprFamily>),
        Floor(R::Ref<'a, FloatExprFamily>),
    }

    pub enum FloatExpr<'a, R: RecursiveRefType + 'a = Storage> {
        Float(R::Value<'a, f64>),
        Add(R::Ref<'a, FloatExprFamily>, R::Ref<'a, FloatExprFamily>),
        Cast(R::Ref<'a, IntExprFamily>),
    }

    pub struct IntExprFamily;

    pub struct FloatExprFamily;

    impl RecursiveFamilyGAT for IntExprFamily {
        type Builder<'a> = IntExpr<'a, BuilderRefType>;

        type Storage<'a> = IntExpr<'a, Storage>;

        type Container<'a> = ExprContainer<'a>;

        type Visiting<'a, Container> = IntExpr<'a, Visiting<Container>>
        where
            Container: 'a;
    }

    impl RecursiveFamily for IntExprFamily {
        fn builder_to_storage<'a>(
            builder_obj: Self::Builder<'a>,
            new_pos: usize,
        ) -> Self::Storage<'a> {
            match builder_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_storage(new_pos), b.to_storage(new_pos)),
                IntExpr::Floor(f) => IntExpr::Floor(f.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'a, 'b, Container>(
            storage_obj: &'b Self::Storage<'a>,
            view: &'b [Container],
        ) -> Self::Visiting<'b, Container>
        where
            Container: 'b,
        {
            match storage_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_visiting(view), b.to_visiting(view)),
                IntExpr::Floor(f) => IntExpr::Floor(f.to_visiting(view)),
            }
        }
    }

    impl RecursiveFamilyGAT for FloatExprFamily {
        type Builder<'a> = FloatExpr<'a, BuilderRefType>;

        type Storage<'a> = FloatExpr<'a, Storage>;

        type Container<'a> = ExprContainer<'a>;

        type Visiting<'a, Container> = FloatExpr<'a, Visiting<Container>>
        where
            Container: 'a;
    }

    impl RecursiveFamily for FloatExprFamily {
        fn builder_to_storage<'a>(
            builder_obj: Self::Builder<'a>,
            new_pos: usize,
        ) -> Self::Storage<'a> {
            match builder_obj {
                FloatExpr::Float(val) => FloatExpr::Float(val),
                FloatExpr::Add(a, b) => {
                    FloatExpr::Add(a.to_storage(new_pos), b.to_storage(new_pos))
                }
                FloatExpr::Cast(i) => FloatExpr::Cast(i.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'a, 'b, Container>(
            storage_obj: &'b Self::Storage<'a>,
            view: &'b [Container],
        ) -> Self::Visiting<'b, Container>
        where
            Container: 'b,
        {
            match storage_obj {
                FloatExpr::Float(val) => FloatExpr::Float(val),
                FloatExpr::Add(a, b) => FloatExpr::Add(a.to_visiting(view), b.to_visiting(view)),
                FloatExpr::Cast(i) => FloatExpr::Cast(i.to_visiting(view)),
            }
        }
    }

    // Macro would probably generate IntExprContainer and
    // FloatExprContainer separately, but that would be inconvenient
    // to write out for the API testing
    pub enum ExprContainer<'a> {
        IntExpr(IntExpr<'a, Storage>),
        FloatExpr(FloatExpr<'a, Storage>),
    }

    impl<'a> From<IntExpr<'a, Storage>> for ExprContainer<'a> {
        fn from(val: IntExpr<'a, Storage>) -> Self {
            ExprContainer::IntExpr(val)
        }
    }

    impl<'a> From<FloatExpr<'a, Storage>> for ExprContainer<'a> {
        fn from(val: FloatExpr<'a, Storage>) -> Self {
            ExprContainer::FloatExpr(val)
        }
    }

    impl<'a> TryAsRef<IntExpr<'a, Storage>> for ExprContainer<'a> {
        type Error = graph::Error;
        fn try_as_ref(&self) -> Result<&IntExpr<'a, Storage>, Self::Error> {
            match self {
                ExprContainer::IntExpr(val) => Ok(val),
                ExprContainer::FloatExpr(_) => Err(graph::Error::IncorrectType {
                    expected: "IntExpr",
                    actual: "FloatExpr",
                }),
            }
        }
    }

    impl<'a> TryAsRef<FloatExpr<'a>> for ExprContainer<'a> {
        type Error = graph::Error;
        fn try_as_ref(&self) -> Result<&FloatExpr<'a>, Self::Error> {
            match self {
                ExprContainer::FloatExpr(val) => Ok(val),
                ExprContainer::IntExpr(_) => Err(graph::Error::IncorrectType {
                    expected: "FloatExpr",
                    actual: "IntExpr",
                }),
            }
        }
    }

    impl<'a> BuilderObj<'a> for IntExpr<'a, BuilderRefType> {
        type Family = IntExprFamily;
    }

    impl<'a> BuilderObj<'a> for FloatExpr<'a, BuilderRefType> {
        type Family = FloatExprFamily;
    }

    impl<'a: 'b, 'b, Container: 'a> Display for IntExpr<'b, Visiting<Container>>
    where
        Container: TryAsRef<IntExpr<'a>>,
        Container: TryAsRef<FloatExpr<'a>>,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                IntExpr::Int(val) => write!(f, "{val}"),
                IntExpr::IntRef(external_val) => write!(f, "{external_val}"),
                IntExpr::Add(a, b) => {
                    write!(f, "{} + {}", a.borrow(), b.borrow())
                }
                IntExpr::Floor(a) => {
                    write!(f, "⌊{}⌋", a.borrow())
                }
            }
        }
    }

    impl<'a: 'b, 'b, Container: 'a> Display for FloatExpr<'b, Visiting<Container>>
    where
        Container: TryAsRef<IntExpr<'a>>,
        Container: TryAsRef<FloatExpr<'a>>,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                FloatExpr::Float(val) => write!(f, "{val}"),
                FloatExpr::Add(a, b) => {
                    write!(f, "{} + {}", a.borrow(), b.borrow())
                }
                FloatExpr::Cast(val) => {
                    write!(f, "float({})", val.borrow())
                }
            }
        }
    }

    impl<'a: 'b, 'b, Container: 'a> std::cmp::PartialEq for IntExpr<'b, Visiting<Container>>
    where
        Container: TryAsRef<IntExpr<'a>>,
        Container: TryAsRef<FloatExpr<'a>>,
    {
        fn eq(&self, rhs: &Self) -> bool {
            match (self, rhs) {
                (IntExpr::Int(a), IntExpr::Int(b)) => a == b,
                (IntExpr::IntRef(a), IntExpr::IntRef(b)) => a == b,
                (IntExpr::Add(a, b), IntExpr::Add(c, d)) => (a == c) && (b == d),
                (IntExpr::Floor(a), IntExpr::Floor(b)) => a == b,
                _ => false,
            }
        }
    }

    impl<'a: 'b, 'b, Container: 'a> std::cmp::PartialEq for FloatExpr<'b, Visiting<Container>>
    where
        Container: TryAsRef<IntExpr<'a>>,
        Container: TryAsRef<FloatExpr<'a>>,
    {
        fn eq(&self, rhs: &Self) -> bool {
            match (self, rhs) {
                (FloatExpr::Float(a), FloatExpr::Float(b)) => a == b,
                (FloatExpr::Add(a, b), FloatExpr::Add(c, d)) => (a == c) && (b == d),
                (FloatExpr::Cast(a), FloatExpr::Cast(b)) => a == b,
                _ => false,
            }
        }
    }

    // impl<'a: 'b, 'b, Container> std::hash::Hash for IntExpr<'a, 'b, Visiting<Container>>
    // where
    //     Container: TryAsRef<IntExpr<'a, 'a>>,
    //     Container: TryAsRef<FloatExpr<'a, 'a>>,
    //     f64: std::hash::Hash,
    // {
    //     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    //         std::mem::discriminant(self).hash(state);
    //         match self {
    //             IntExpr::Int(a) => a.hash(state),
    //             IntExpr::IntRef(a) => a.hash(state),
    //             IntExpr::Add(a, b) => {
    //                 a.hash(state);
    //                 b.hash(state);
    //             }
    //             IntExpr::Floor(a) => a.hash(state),
    //         }
    //     }
    // }

    // impl<'a: 'b, 'b, Container> std::hash::Hash for FloatExpr<'a, 'b, Visiting<Container>>
    // where
    //     Container: TryAsRef<IntExpr<'a, 'a>>,
    //     Container: TryAsRef<FloatExpr<'a, 'a>>,
    //     f64: std::hash::Hash,
    // {
    //     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    //         std::mem::discriminant(self).hash(state);
    //         match self {
    //             FloatExpr::Float(a) => a.hash(state),
    //             FloatExpr::Add(a, b) => {
    //                 a.hash(state);
    //                 b.hash(state);
    //             }
    //             FloatExpr::Cast(a) => a.hash(state),
    //         }
    //     }
    // }
}

impl<'a, Container: 'a> expr::IntExpr<'a, graph2::Visiting<Container>>
where
    Container: TryAsRef<expr::IntExpr<'a>>,
    Container: TryAsRef<expr::FloatExpr<'a>>,
{
    fn eval(self) -> i64 {
        match self {
            Self::Int(&val) => val,
            Self::IntRef(&val) => *val,
            Self::Add(a, b) => a.borrow().eval() + b.borrow().eval(),
            Self::Floor(a) => a.borrow().eval().floor() as i64,
        }
    }
}

impl<'a, Container: 'a> expr::FloatExpr<'a, graph2::Visiting<Container>>
where
    Container: TryAsRef<expr::IntExpr<'a>>,
    Container: TryAsRef<expr::FloatExpr<'a>>,
{
    fn eval(self) -> f64 {
        match self {
            Self::Float(&val) => val,
            Self::Add(a, b) => a.borrow().eval() + b.borrow().eval(),
            Self::Cast(a) => a.borrow().eval() as f64,
        }
    }
}

#[test]
fn eval_int_expr() -> Result<(), graph::Error> {
    use expr::IntExpr;
    let expr = TypedTree::build(|builder| {
        let a = builder.push(IntExpr::Int(5));
        let b = builder.push(IntExpr::Int(10));
        let c = builder.push(IntExpr::Add(a, b));
        let d = builder.push(IntExpr::Int(100));
        builder.push(IntExpr::Add(c, d))
    });

    let value: i64 = expr.root().borrow().eval();

    assert_eq!(value, 115);

    Ok(())
}

#[test]
fn eval_int_expr_with_reference() -> Result<(), graph::Error> {
    use expr::IntExpr;

    let data = vec![5, 10, 100];

    let expr = TypedTree::build(|builder| {
        let a = builder.push(IntExpr::IntRef(&data[0]));
        let b = builder.push(IntExpr::IntRef(&data[1]));
        let c = builder.push(IntExpr::Add(a, b));
        let d = builder.push(IntExpr::IntRef(&data[2]));
        builder.push(IntExpr::Add(c, d))
    });

    let value: i64 = expr.root().borrow().eval();

    assert_eq!(value, 115);

    Ok(())
}

#[test]
fn int_self_ref_equal() {
    use expr::IntExpr;

    let expr1 = TypedTree::build(|builder| builder.push(IntExpr::Int(5)));

    assert!(expr1.root().ref_equals(expr1.root()));
}

#[test]
fn int_equivalent_tree_not_ref_equal() {
    use expr::IntExpr;

    let expr1 = TypedTree::build(|builder| builder.push(IntExpr::Int(5)));
    let expr2 = TypedTree::build(|builder| builder.push(IntExpr::Int(5)));

    assert!(!expr1.root().ref_equals(expr2.root()));
}

#[test]
fn int_equivalent_tree_structurally_equal() {
    use expr::{IntExpr, IntExprFamily};
    use graph2::{Builder, TypedTree};

    let expr1: TypedTree<IntExprFamily> = {
        let mut builder = Builder::new();
        let a = builder.push(IntExpr::Int(5));
        builder.finalize(a)
    };
    let expr2: TypedTree<IntExprFamily> = {
        let mut builder = Builder::new();
        let a = builder.push(IntExpr::Int(5));
        builder.finalize(a)
    };
    assert!(expr1.root() == expr2.root());
}

#[test]
fn display_() {
    use expr::IntExpr;

    let expr = TypedTree::build(|builder| {
        let a = builder.push(IntExpr::Int(5));
        let b = builder.push(IntExpr::Int(10));
        builder.push(IntExpr::Add(a, b))
    });

    assert_eq!(format!("{}", expr.root().borrow()), "5 + 10");
    assert_eq!(format!("{}", expr.root()), "5 + 10");
    assert_eq!(format!("{expr}"), "5 + 10");
}

#[test]
fn mixed_type_expression() {
    use expr::{FloatExpr, IntExpr};

    let data = vec![5, 10, 100];

    let expr = TypedTree::build(|builder| {
        let a = builder.push(FloatExpr::Float(5.6));
        let b = builder.push(FloatExpr::Float(10.6));
        let c = builder.push(FloatExpr::Add(a, b));
        let d = builder.push(IntExpr::Floor(c));
        let e = builder.push(IntExpr::IntRef(&data[2]));
        builder.push(IntExpr::Add(d, e))
    });

    let value: i64 = expr.root().borrow().eval();

    assert_eq!(value, 116);
}
