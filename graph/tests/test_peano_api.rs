#![allow(dead_code)]

mod graph2 {
    use std::fmt::{Display, Formatter};
    use std::marker::PhantomData;

    ///////////////////////////////////////////////////////////
    ////////////// Enable self-referential types //////////////
    ///////////////////////////////////////////////////////////

    /// Usage type (e.g. Storage, Builder, Visitor)
    pub trait RecursiveRefType {
        type Ref<'a: 'b, 'b, Family>
        where
            Family: RecursiveFamily + 'a;
        type Value<'a, T: 'a>;
    }

    /// Meta-object, at compile-time can generate an enum for a
    /// specific usage type.  At runtime, can convert between enums
    /// of different usage types.
    pub trait RecursiveFamily: Sized {
        type Builder<'a>
        where
            Self: 'a;

        type Storage<'a>
        where
            Self: 'a;

        type Container<'a>: ContainerOf<Self::Storage<'a>>
        where
            Self: 'a;

        type Visiting<'a: 'b, 'b>
        where
            Self: 'a;

        fn builder_to_storage<'a>(
            builder_obj: Self::Builder<'a>,
            new_pos: usize,
        ) -> Self::Storage<'a>;

        fn storage_to_container<'a>(storage_obj: Self::Storage<'a>) -> Self::Container<'a>;

        fn container_to_storage<'a: 'b, 'b>(
            container: &'b Self::Container<'a>,
        ) -> Result<&'b Self::Storage<'a>, graph::Error>;

        fn storage_to_visiting<'a: 'b, 'b>(
            storage_obj: &'b Self::Storage<'a>,
            view: &'b [Self::Container<'a>],
        ) -> Self::Visiting<'a, 'b>;
    }

    /// A recursive object, belonging to a specific family of
    /// recursive types, with a specific type in that family.  This
    /// trait is automatically implemented, and is used for functions
    /// that must derive their types from arguments, and return
    /// another type in the same family.  (e.g. `Builder::push`
    /// accepts an argument of type `F::Obj<Builder>` and must
    /// internally convert it to an object of type `F::Obj<Storage>`)
    pub trait RecursiveObj {
        type Family: RecursiveFamily;
        type RefType: RecursiveRefType;
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

    impl RecursiveRefType for Storage {
        type Ref<'a: 'b, 'b, Family: RecursiveFamily + 'a> = StorageRef<Family>;
        type Value<'a, T: 'a> = T;
    }

    /// Represents a reference in the linearized structure.
    pub struct StorageRef<Family: RecursiveFamily> {
        /// Position of the referred-to type, relative to the position
        /// of the `RecursiveFamily::Obj<Storage>` that holds the
        /// `StorageRef`.
        rel_pos: usize,
        _node: PhantomData<*const Family>,
    }

    impl<Family: RecursiveFamily> StorageRef<Family> {
        pub fn to_visiting<'a: 'b, 'b>(
            &'b self,
            view: &'b [Family::Container<'a>],
        ) -> VisitingRef<'a, 'b, Family> {
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
    pub struct TypedTree<'a, Family: RecursiveFamily + 'a> {
        nodes: Vec<Family::Container<'a>>,
    }

    impl<'a, Family: RecursiveFamily + 'a> TypedTree<'a, Family> {
        /// Returns a reference to the root node of the tree
        pub fn root<'b>(&'b self) -> VisitingRef<'a, 'b, Family>
        where
            'a: 'b,
        {
            VisitingRef { view: &self.nodes }
        }
    }

    impl<'a, Family: RecursiveFamily + 'a> Display for TypedTree<'a, Family>
    where
        for<'b> Family::Visiting<'a, 'b>: Display,
    {
        fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
            write!(f, "{}", self.root())
        }
    }

    /////////////////////////////////////
    ////////////// Builder //////////////
    /////////////////////////////////////

    /// A constructor used to generate a `TypedTree<Container>`.
    pub struct Builder;

    pub struct BuilderObj<'a, Family: RecursiveFamily + 'a> {
        nodes: Vec<Family::Container<'a>>,
    }

    impl Builder {
        /// Constructs an empty `Builder`.
        pub fn new<'a, Family: RecursiveFamily>() -> BuilderObj<'a, Family> {
            BuilderObj { nodes: Vec::new() }
        }
    }

    /// Reference type used while building a tree.  Any time the user
    /// pushes a node into the builder, they receive a reference.
    /// That reference may then be used to construct additional
    /// builder nodes.
    pub struct BuilderRef<Family: RecursiveFamily> {
        abs_pos: usize,
        _node: PhantomData<*const Family>,
    }

    impl RecursiveRefType for Builder {
        type Ref<'a: 'b, 'b, Family: RecursiveFamily + 'a> = BuilderRef<Family>;
        type Value<'a, T: 'a> = T;
    }

    impl<'a, Family: RecursiveFamily> BuilderObj<'a, Family> {
        /// Insert a new node to the builder
        pub fn push<SubFamily: RecursiveFamily<Container<'a> = Family::Container<'a>> + 'a>(
            &mut self,
            builder_obj: Family::Builder<'a>,
        ) -> BuilderRef<SubFamily> {
            let abs_pos = self.nodes.len();
            let storage_obj = Family::builder_to_storage(builder_obj, abs_pos);
            let container = Family::storage_to_container(storage_obj);
            self.nodes.push(container);
            BuilderRef {
                abs_pos,
                _node: PhantomData,
            }
        }

        pub fn finalize(self, top_node: BuilderRef<Family>) -> TypedTree<'a, Family> {
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

    impl<Family: RecursiveFamily> BuilderRef<Family> {
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

    pub struct Visiting;

    pub struct VisitingRef<'a: 'b, 'b, Family: RecursiveFamily + 'a> {
        view: &'b [Family::Container<'a>],
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily> Clone for VisitingRef<'a, 'b, Family> {
        fn clone(&self) -> Self {
            VisitingRef { view: self.view }
        }
    }
    impl<'a: 'b, 'b, Family: RecursiveFamily> Copy for VisitingRef<'a, 'b, Family> {}

    impl RecursiveRefType for Visiting {
        type Ref<'a: 'b, 'b, Family: RecursiveFamily + 'a> = VisitingRef<'a, 'b, Family>;
        type Value<'b, T: 'b> = &'b T;
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily + 'a> VisitingRef<'a, 'b, Family> {
        pub fn borrow(self) -> Result<Family::Visiting<'a, 'b>, graph::Error> {
            let container: &Family::Container<'a> =
                self.view.last().ok_or(graph::Error::EmptyExpression)?;
            let node: &Family::Storage<'a> = Family::container_to_storage(container)?;
            let view = self.view;
            let live_ref = Family::storage_to_visiting(node, view);
            Ok(live_ref)
        }

        pub fn ref_equals(&self, other: Self) -> bool {
            self.view.as_ptr_range() == other.view.as_ptr_range()
        }

        pub fn structural_equals(&self, _other: VisitingRef<'a, 'b, Family>) -> bool {
            todo!()
        }
    }

    impl<'a: 'b, 'b, Family: RecursiveFamily> Display for VisitingRef<'a, 'b, Family>
    where
        Family: RecursiveFamily,
        Family::Visiting<'a, 'b>: Display,
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

    pub enum Number<'a: 'b, 'b, R: RecursiveRefType + 'a = Storage> {
        Zero,
        Successor(R::Ref<'a, 'b, NumberFamily>),
    }

    pub struct NumberFamily;

    impl RecursiveFamily for NumberFamily {
        type Builder<'a> = Number<'a, 'a, Builder>;

        type Storage<'a> = Number<'a, 'a, Storage>;

        type Container<'a> = NumberContainer<'a>;

        type Visiting<'a: 'b, 'b> = Number<'a, 'b, Visiting>;

        fn builder_to_storage<'a>(
            builder_obj: Self::Builder<'a>,
            new_pos: usize,
        ) -> Self::Storage<'a> {
            match builder_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_storage(new_pos)),
            }
        }

        fn storage_to_container<'a>(storage_obj: Self::Storage<'a>) -> Self::Container<'a> {
            Self::Container::<'a>::to_container(storage_obj)
        }

        fn container_to_storage<'a: 'b, 'b>(
            container: &'b Self::Container<'a>,
        ) -> Result<&'b Self::Storage<'a>, graph::Error> {
            Self::Container::<'a>::from_container(container)
        }

        fn storage_to_visiting<'a: 'b, 'b>(
            storage_obj: &'b Self::Storage<'a>,
            view: &'b [Self::Container<'a>],
        ) -> Self::Visiting<'a, 'b> {
            match storage_obj {
                Number::Zero => Number::Zero,
                Number::Successor(old_ref) => Number::Successor(old_ref.to_visiting(view)),
            }
        }
    }

    impl<'a: 'b, 'b, RefType: RecursiveRefType> RecursiveObj for Number<'a, 'b, RefType> {
        type Family = NumberFamily;
        type RefType = RefType;
    }

    pub enum NumberContainer<'a> {
        Number(Number<'a, 'a, Storage>),
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

impl<'a> graph2::TypedTree<'a, peano::NumberFamily> {
    fn new(val: u8) -> Self {
        let mut builder = graph2::Builder::new();
        let mut a = builder.push(peano::Number::Zero);
        for _ in 0..val {
            a = builder.push(peano::Number::Successor(a));
        }
        builder.finalize(a)
    }
}

impl<'a: 'b, 'b> peano::Number<'a, 'b, graph2::Visiting> {
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
    let _three: graph2::TypedTree<peano::NumberFamily> = {
        let mut builder = graph2::Builder::new::<peano::NumberFamily>();
        let mut a: graph2::BuilderRef<peano::NumberFamily> =
            builder.push::<peano::NumberFamily>(peano::Number::<graph2::Builder>::Zero);
        for _ in 0..3 {
            a = builder.push::<peano::NumberFamily>(peano::Number::<graph2::Builder>::Successor(a));
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
    assert!(matches!(zero.root().borrow()?, Number::Zero));
    assert!(matches!(one.root().borrow()?, Number::Successor(_)));

    Ok(())
}

#[test]
fn match_live_ref() -> Result<(), graph::Error> {
    let three = graph2::TypedTree::<peano::NumberFamily>::new(3);

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
    let three = graph2::TypedTree::<peano::NumberFamily>::new(3);
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

    pub enum IntExpr<'a: 'b, 'b, R: RecursiveRefType + 'a = Storage> {
        Int(R::Value<'b, i64>),
        IntRef(R::Value<'b, &'b i64>),
        Add(R::Ref<'a, 'b, IntExprFamily>, R::Ref<'a, 'b, IntExprFamily>),
    }

    pub struct IntExprFamily;

    impl RecursiveFamily for IntExprFamily {
        type Builder<'a> = IntExpr<'a, 'a, Builder>;

        type Storage<'a> = IntExpr<'a, 'a, Storage>;

        type Container<'a> = IntExprContainer<'a>;

        type Visiting<'a: 'b, 'b> = IntExpr<'a, 'b, Visiting>;

        fn builder_to_storage<'a>(
            builder_obj: Self::Builder<'a>,
            new_pos: usize,
        ) -> Self::Storage<'a> {
            match builder_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_storage(new_pos), b.to_storage(new_pos)),
            }
        }

        fn storage_to_container<'a>(storage_obj: Self::Storage<'a>) -> Self::Container<'a> {
            Self::Container::<'a>::to_container(storage_obj)
        }

        fn container_to_storage<'a: 'b, 'b>(
            container: &'b Self::Container<'a>,
        ) -> Result<&'b Self::Storage<'a>, graph::Error> {
            Self::Container::<'a>::from_container(container)
        }

        fn storage_to_visiting<'a: 'b, 'b>(
            storage_obj: &'b Self::Storage<'a>,
            view: &'b [Self::Container<'a>],
        ) -> Self::Visiting<'a, 'b> {
            match storage_obj {
                IntExpr::Int(val) => IntExpr::Int(val),
                IntExpr::IntRef(val) => IntExpr::IntRef(val),
                IntExpr::Add(a, b) => IntExpr::Add(a.to_visiting(view), b.to_visiting(view)),
            }
        }
    }

    impl<'a: 'b, 'b, RefType: RecursiveRefType> RecursiveObj for IntExpr<'a, 'b, RefType> {
        type Family = IntExprFamily;
        type RefType = RefType;
    }

    pub enum IntExprContainer<'a> {
        IntExpr(IntExpr<'a, 'a, Storage>),
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

    impl<'a: 'b, 'b> Display for IntExpr<'a, 'b, Visiting> {
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

impl<'a: 'b, 'b> direct_expr::IntExpr<'a, 'b, graph2::Visiting> {
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
    use direct_expr::{IntExpr, IntExprFamily};
    let expr: graph2::TypedTree<IntExprFamily> = {
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
    use direct_expr::{IntExpr, IntExprFamily};

    let data = vec![5, 10, 100];

    let expr: graph2::TypedTree<IntExprFamily> = {
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
    use direct_expr::{IntExpr, IntExprFamily};
    use graph2::{Builder, TypedTree};

    let expr1: TypedTree<IntExprFamily> = {
        let mut builder = Builder::new();
        let a = builder.push(IntExpr::Int(5));
        builder.finalize(a)
    };
    assert!(expr1.root().ref_equals(expr1.root()));
}

#[test]
fn int_equivalent_tree_not_ref_equal() {
    use direct_expr::{IntExpr, IntExprFamily};
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
    use direct_expr::{IntExpr, IntExprFamily};
    use graph2::{Builder, TypedTree};

    let expr: TypedTree<IntExprFamily> = {
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
