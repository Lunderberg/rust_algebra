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
use graph2::TypedTree;

mod graph2 {
    use std::fmt::{Display, Formatter};
    use std::marker::PhantomData;

    ///////////////////////////////////////////////////////////
    ////////////// Enable self-referential types //////////////
    ///////////////////////////////////////////////////////////

    /// Usage type (e.g. Storage, Builder, Visitor)
    pub trait RecursiveRefType<'ext> {
        type Ref<Family: RecursiveFamily + 'ext>;
        type Value<T: 'ext>;
    }

    /// Meta-object, at compile-time can generate an enum for a
    /// specific usage type.  At runtime, can convert between enums
    /// of different usage types.
    pub trait RecursiveFamily {
        type Builder: BuilderObj<Family = Self>;

        // The requirement of `Into<Self::Container>` should already
        // be provided by the requirement of `Container:
        // From<Self::Storage>`.  However, in some cases, type
        // normalization may prevent the implied bound from being
        // used.
        //
        // Related:
        // https://stackoverflow.com/q/44790642,
        // https://github.com/rust-lang/rust/issues/105948
        type Storage: Into<Self::Container>;

        type Container: TryAsRef<Self::Storage> + From<Self::Storage>;

        type Visiting<'view>: 'view
        where
            Self: 'view;

        fn builder_to_storage(builder_obj: Self::Builder, new_pos: usize) -> Self::Storage;

        fn storage_to_visiting<'view>(
            storage_obj: &'view Self::Storage,
            view: &'view [Self::Container],
        ) -> Self::Visiting<'view>
        where
            Self: 'view;
    }

    pub trait TryAsRef<T> {
        type Error: std::fmt::Debug;
        fn try_as_ref(&self) -> Result<&T, Self::Error>;
    }

    pub trait BuilderObj {
        type Family: RecursiveFamily<Builder = Self>;
    }

    /////////////////////////////////////
    ////////////// Storage //////////////
    /////////////////////////////////////

    /// A usage annotation for objects that may be stored in the
    /// linearized structure.
    pub struct Storage;

    impl<'ext> RecursiveRefType<'ext> for Storage {
        type Ref<Family: RecursiveFamily + 'ext> = StorageRef<Family>;
        type Value<T: 'ext> = T;
    }

    /// Represents a reference in the linearized structure.
    pub struct StorageRef<Family> {
        /// Position of the referred-to type, relative to the position
        /// of the `RecursiveFamily::Obj<Storage>` that holds the
        /// `StorageRef`.
        rel_pos: usize,
        _node: PhantomData<*const Family>,
    }

    impl<Family: RecursiveFamily<Container = Container>, Container> StorageRef<Family> {
        pub fn to_visiting<'b>(&'b self, view: &'b [Container]) -> VisitingRef<'b, Family> {
            let index = view.len().checked_sub(self.rel_pos).unwrap_or(0);
            VisitingRef {
                view: &view[..index],
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
    pub struct TypedTree<Family: RecursiveFamily> {
        nodes: Vec<Family::Container>,
    }

    impl<Family: RecursiveFamily> TypedTree<Family> {
        pub fn build<Func, Container>(func: Func) -> Self
        where
            Family: RecursiveFamily<Container = Container>,
            Func: FnOnce(&mut Builder<Container>) -> BuilderRef<Family>,
        {
            let mut builder = Builder::new();
            let top_node = func(&mut builder);
            builder.finalize(top_node)
        }

        /// Returns a reference to the root node of the tree
        pub fn root<'b>(&'b self) -> VisitingRef<'b, Family> {
            VisitingRef { view: &self.nodes }
        }
    }

    impl<Family: RecursiveFamily> Display for TypedTree<Family>
    where
        for<'b> Family::Visiting<'b>: Display,
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

    impl<'ext> RecursiveRefType<'ext> for BuilderRefType {
        type Ref<Family: RecursiveFamily + 'ext> = BuilderRef<Family>;
        type Value<T: 'ext> = T;
    }

    impl<Container> Builder<Container> {
        /// Insert a new node to the builder
        pub fn push<
            Obj: BuilderObj<Family = Family>,
            Family: RecursiveFamily<Builder = Obj, Container = Container>,
        >(
            &mut self,
            builder_obj: Obj,
        ) -> BuilderRef<Obj::Family> {
            let abs_pos = self.nodes.len();
            let storage_obj: Family::Storage =
                Obj::Family::builder_to_storage(builder_obj, abs_pos);
            let container_obj: Family::Container = storage_obj.into();
            self.nodes.push(container_obj);
            BuilderRef {
                abs_pos,
                _node: PhantomData,
            }
        }

        pub fn finalize<Family: RecursiveFamily<Container = Container>>(
            self,
            top_node: BuilderRef<Family>,
        ) -> TypedTree<Family> {
            let mut nodes = self.nodes;

            // These should usually be no-ops, since passing something
            // other than the most recent reference would mean that
            // previously generated nodes have no effect.  However, in
            // that case, the truncate() call is necessary for correct
            // behavior.
            nodes.truncate(top_node.abs_pos + 1);
            nodes.shrink_to_fit();

            TypedTree { nodes }
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

    pub struct Visiting<'view> {
        _phantom: PhantomData<&'view ()>,
    }

    pub struct VisitingRef<'a, Family: RecursiveFamily> {
        view: &'a [Family::Container],
    }

    impl<'a, Family: RecursiveFamily> Clone for VisitingRef<'a, Family> {
        fn clone(&self) -> Self {
            VisitingRef { view: self.view }
        }
    }
    impl<'a, Family: RecursiveFamily> Copy for VisitingRef<'a, Family> {}

    impl<'ext: 'view, 'view> RecursiveRefType<'ext> for Visiting<'view> {
        type Ref<Family: RecursiveFamily + 'ext> = VisitingRef<'view, Family>;
        type Value<T: 'ext> = &'view T;
    }

    impl<'a, Family: RecursiveFamily> VisitingRef<'a, Family> {
        pub fn borrow(self) -> Family::Visiting<'a> {
            let container: &Family::Container = self
                .view
                .last()
                .ok_or(graph::Error::EmptyExpression)
                .expect("Malformed tree found with empty node");
            let node: &Family::Storage = container
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

    impl<'a, Family: RecursiveFamily + 'a> std::cmp::PartialEq for VisitingRef<'a, Family>
    where
        Family::Visiting<'a>: PartialEq,
    {
        fn eq(&self, rhs: &Self) -> bool {
            self.ref_equals(*rhs) || (self.borrow() == rhs.borrow())
        }
    }

    impl<'a, Family: RecursiveFamily + 'a> std::hash::Hash for VisitingRef<'a, Family>
    where
        Family::Visiting<'a>: std::hash::Hash,
    {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.borrow().hash(state)
        }
    }

    impl<'a, Family: RecursiveFamily + 'a> Display for VisitingRef<'a, Family>
    where
        Family::Visiting<'a>: Display,
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

    pub enum Number<R: RecursiveRefType<'static> = Storage> {
        Zero,
        Successor(R::Ref<NumberFamily>),
    }

    pub struct NumberFamily;

    impl RecursiveFamily for NumberFamily {
        type Builder = Number<BuilderRefType>;

        type Storage = Number<Storage>;

        type Container = NumberContainer;

        type Visiting<'view> = Number<Visiting<'view>>;

        fn builder_to_storage(builder_obj: Self::Builder, new_pos: usize) -> Self::Storage {
            match builder_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'view>(
            storage_obj: &'view Self::Storage,
            view: &'view [Self::Container],
        ) -> Self::Visiting<'view>
        where
            Self: 'view,
        {
            match storage_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_visiting(view)),
            }
        }
    }

    impl BuilderObj for Number<BuilderRefType> {
        type Family = NumberFamily;
    }

    pub enum NumberContainer {
        Number(Number<Storage>),
    }

    impl<'a> From<Number<Storage>> for NumberContainer {
        fn from(val: Number<Storage>) -> Self {
            NumberContainer::Number(val)
        }
    }

    impl TryAsRef<Number<Storage>> for NumberContainer {
        type Error = graph::Error;
        fn try_as_ref(&self) -> Result<&Number<Storage>, Self::Error> {
            match self {
                NumberContainer::Number(val) => Ok(val),
            }
        }
    }

    impl<'b> std::cmp::PartialEq for Number<Visiting<'b>> {
        fn eq(&self, rhs: &Self) -> bool {
            match (self, rhs) {
                (Number::Zero, Number::Zero) => true,
                (Number::Successor(a), Number::Successor(b)) => a == b,
                _ => false,
            }
        }
    }

    impl<'b> std::hash::Hash for Number<Visiting<'b>> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            std::mem::discriminant(self).hash(state);
            match self {
                Number::Zero => {}
                Number::Successor(prev) => prev.hash(state),
            }
        }
    }
}

impl graph2::TypedTree<peano::NumberFamily> {
    fn new(val: u8) -> Self {
        let mut builder = graph2::Builder::new();
        let mut a = builder.push(peano::Number::Zero);
        for _ in 0..val {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.finalize(a)
    }
}

impl<'b> peano::Number<graph2::Visiting<'b>> {
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
    let _three: graph2::TypedTree<peano::NumberFamily> = {
        let mut builder = graph2::Builder::<peano::NumberContainer>::new();
        let mut a: graph2::BuilderRef<peano::NumberFamily> =
            builder.push::<peano::Number<graph2::BuilderRefType>, peano::NumberFamily>(
                peano::Number::<graph2::BuilderRefType>::Zero,
            );
        for _ in 0..3 {
            a = builder.push::<peano::Number<graph2::BuilderRefType>, peano::NumberFamily>(
                peano::Number::<graph2::BuilderRefType>::Successor(a),
            );
        }
        builder.finalize(a)
    };
}

#[test]
fn construct_unannotated() {
    let _three: graph2::TypedTree<peano::NumberFamily> = {
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
    let _three = graph2::TypedTree::<peano::NumberFamily>::new(3);
}

#[test]
fn match_root() -> Result<(), graph::Error> {
    let zero = graph2::TypedTree::<peano::NumberFamily>::new(0);
    let one = graph2::TypedTree::<peano::NumberFamily>::new(1);

    use peano::Number;
    assert!(matches!(zero.root().borrow(), Number::Zero));
    assert!(matches!(one.root().borrow(), Number::Successor(_)));

    Ok(())
}

#[test]
fn match_live_ref() -> Result<(), graph::Error> {
    let three = graph2::TypedTree::<peano::NumberFamily>::new(3);

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
    let three = graph2::TypedTree::<peano::NumberFamily>::new(3);
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

    pub enum IntExpr<'a, R: RecursiveRefType<'a> = Storage> {
        Int(R::Value<i64>),
        IntRef(R::Value<&'a i64>),
        Add(R::Ref<IntExprFamily<'a>>, R::Ref<IntExprFamily<'a>>),
        Floor(R::Ref<FloatExprFamily<'a>>),
    }

    pub enum FloatExpr<'a, R: RecursiveRefType<'a> = Storage> {
        Float(R::Value<f64>),
        Add(R::Ref<FloatExprFamily<'a>>, R::Ref<FloatExprFamily<'a>>),
        Cast(R::Ref<IntExprFamily<'a>>),
    }

    pub struct IntExprFamily<'a> {
        _phantom: std::marker::PhantomData<&'a ()>,
    }

    pub struct FloatExprFamily<'a> {
        _phantom: std::marker::PhantomData<&'a ()>,
    }

    impl<'a> RecursiveFamily for IntExprFamily<'a> {
        type Builder = IntExpr<'a, BuilderRefType>;

        type Storage = IntExpr<'a, Storage>;

        type Container = ExprContainer<'a>;

        type Visiting<'view> = IntExpr<'a, Visiting<'view>>
        where
            'a:'view;

        fn builder_to_storage(builder_obj: Self::Builder, new_pos: usize) -> Self::Storage {
            match builder_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_storage(new_pos), b.to_storage(new_pos)),
                IntExpr::Floor(f) => IntExpr::Floor(f.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'view>(
            storage_obj: &'view Self::Storage,
            view: &'view [Self::Container],
        ) -> Self::Visiting<'view>
        where
            'a: 'view,
        {
            match storage_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_visiting(view), b.to_visiting(view)),
                IntExpr::Floor(f) => IntExpr::Floor(f.to_visiting(view)),
            }
        }
    }

    impl<'a> RecursiveFamily for FloatExprFamily<'a> {
        type Builder = FloatExpr<'a, BuilderRefType>;

        type Storage = FloatExpr<'a, Storage>;

        type Container = ExprContainer<'a>;

        type Visiting<'view> = FloatExpr<'a, Visiting<'view>>
        where
            'a:'view;

        fn builder_to_storage(builder_obj: Self::Builder, new_pos: usize) -> Self::Storage {
            match builder_obj {
                FloatExpr::Float(val) => FloatExpr::Float(val),
                FloatExpr::Add(a, b) => {
                    FloatExpr::Add(a.to_storage(new_pos), b.to_storage(new_pos))
                }
                FloatExpr::Cast(i) => FloatExpr::Cast(i.to_storage(new_pos)),
            }
        }

        fn storage_to_visiting<'view>(
            storage_obj: &'view Self::Storage,
            view: &'view [Self::Container],
        ) -> Self::Visiting<'view>
        where
            'a: 'view,
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

    impl<'a> BuilderObj for IntExpr<'a, BuilderRefType> {
        type Family = IntExprFamily<'a>;
    }

    impl<'a> BuilderObj for FloatExpr<'a, BuilderRefType> {
        type Family = FloatExprFamily<'a>;
    }

    impl<'a: 'view, 'view> Display for IntExpr<'a, Visiting<'view>> {
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

    impl<'a: 'view, 'view> Display for FloatExpr<'a, Visiting<'view>> {
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

    impl<'a: 'view, 'view> std::cmp::PartialEq for IntExpr<'a, Visiting<'view>> {
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

    impl<'a: 'view, 'view> std::cmp::PartialEq for FloatExpr<'a, Visiting<'view>> {
        fn eq(&self, rhs: &Self) -> bool {
            match (self, rhs) {
                (FloatExpr::Float(a), FloatExpr::Float(b)) => a == b,
                (FloatExpr::Add(a, b), FloatExpr::Add(c, d)) => (a == c) && (b == d),
                (FloatExpr::Cast(a), FloatExpr::Cast(b)) => a == b,
                _ => false,
            }
        }
    }

    // impl<'a: 'b, 'b, Container> std::hash::Hash for IntExpr<'a, 'b, Visiting>
    // where
    //     Container: TryAsRef<IntExpr<'a>>,
    //     Container: TryAsRef<FloatExpr<'a>>,
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

    // impl<'a: 'b, 'b, Container> std::hash::Hash for FloatExpr<'a, 'b, Visiting>
    // where
    //     Container: TryAsRef<IntExpr<'a>>,
    //     Container: TryAsRef<FloatExpr<'a>>,
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

impl<'a: 'view, 'view> expr::IntExpr<'a, graph2::Visiting<'view>> {
    fn eval(self) -> i64 {
        match self {
            Self::Int(&val) => val,
            Self::IntRef(&val) => *val,
            Self::Add(a, b) => a.borrow().eval() + b.borrow().eval(),
            Self::Floor(a) => a.borrow().eval().floor() as i64,
        }
    }
}

impl<'a: 'view, 'view> expr::FloatExpr<'a, graph2::Visiting<'view>> {
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
