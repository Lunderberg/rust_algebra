use std::fmt::Debug;
use std::marker::PhantomData;

pub trait TryAsRef<Target> {
    type Error: Debug;
    fn try_as_ref(&self) -> Result<&Target, Self::Error>;
}

impl<Target> TryAsRef<Target> for Target {
    type Error = std::convert::Infallible;

    fn try_as_ref(&self) -> Result<&Target, Self::Error> {
        Ok(self)
    }
}

pub trait ValueRefType<'ext> {
    type Value<Target: 'ext>;
}

pub struct ValueOwner;

pub struct ValueVisitor<'view> {
    phantom: PhantomData<&'view ()>,
}

impl<'ext> ValueRefType<'ext> for ValueOwner {
    type Value<Target: 'ext> = Target;
}
impl<'ext: 'view, 'view> ValueRefType<'ext> for ValueVisitor<'view> {
    type Value<Target: 'ext> = &'view Target;
}

pub trait RefType<'ext>: Sized + Copy + Clone {
    type ValueRef: ValueRefType<'ext>;
    type Node<Target: 'ext>: TypedNodeRef<'ext, Untyped = Self, Target = Target>;
}

pub trait TypedNodeRef<'ext>: Copy + Clone {
    type Untyped: RefType<'ext>;
    type Target: 'ext;

    fn strip_type(&self) -> Self::Untyped;
}

pub struct BuilderRef<Target = ()> {
    abs_pos: usize,
    phantom: PhantomData<Target>,
}

impl<Target> Clone for BuilderRef<Target> {
    fn clone(&self) -> Self {
        Self {
            abs_pos: self.abs_pos,
            phantom: PhantomData,
        }
    }
}
impl<Target> Copy for BuilderRef<Target> {}

impl<'ext> RefType<'ext> for BuilderRef {
    type ValueRef = ValueOwner;
    type Node<Target: 'ext> = BuilderRef<Target>;
}
impl<'ext, Target: 'ext> TypedNodeRef<'ext> for BuilderRef<Target> {
    type Untyped = BuilderRef;
    type Target = Target;

    fn strip_type(&self) -> Self::Untyped {
        BuilderRef {
            abs_pos: self.abs_pos,
            phantom: PhantomData,
        }
    }
}

pub struct StorageRef<Target = ()> {
    rel_pos: usize,
    phantom: PhantomData<Target>,
}
impl<Target> Clone for StorageRef<Target> {
    fn clone(&self) -> Self {
        StorageRef {
            rel_pos: self.rel_pos,
            phantom: PhantomData,
        }
    }
}
impl<Target> Copy for StorageRef<Target> {}

impl<'ext> RefType<'ext> for StorageRef {
    type ValueRef = ValueOwner;
    type Node<Target: 'ext> = StorageRef<Target>;
}
impl<'ext, Target: 'ext> TypedNodeRef<'ext> for StorageRef<Target> {
    type Untyped = StorageRef;
    type Target = Target;

    fn strip_type(&self) -> Self::Untyped {
        StorageRef {
            rel_pos: self.rel_pos,
            phantom: PhantomData,
        }
    }
}

pub struct VisitingRef<'view, Container, Target = ()> {
    view: &'view [Container],
    phantom: PhantomData<&'view Target>,
}
impl<'view, Container, Target> Clone for VisitingRef<'view, Container, Target> {
    fn clone(&self) -> Self {
        Self {
            view: self.view,
            phantom: PhantomData,
        }
    }
}
impl<'view, Container, Target> Copy for VisitingRef<'view, Container, Target> {}
impl<'ext: 'view, 'view, Container> RefType<'ext> for VisitingRef<'view, Container> {
    type ValueRef = ValueVisitor<'view>;
    type Node<Target: 'ext> = VisitingRef<'view, Container, Target>;
}
impl<'ext: 'view, 'view, Container, Target: 'ext> TypedNodeRef<'ext>
    for VisitingRef<'view, Container, Target>
{
    type Untyped = VisitingRef<'view, Container>;
    type Target = Target;

    fn strip_type(&self) -> Self::Untyped {
        VisitingRef {
            view: self.view,
            phantom: PhantomData,
        }
    }
}

pub trait RefConverter<'ext> {
    type FromRef: RefType<'ext>;
    type ToRef: RefType<'ext>;

    fn convert_ref<Target: 'ext>(
        &self,
        from_ref: <Self::FromRef as RefType<'ext>>::Node<Target>,
    ) -> <Self::ToRef as RefType<'ext>>::Node<Target>;
}

struct BuilderToStorage {
    new_storage_pos: usize,
}
impl<'ext> RefConverter<'ext> for BuilderToStorage {
    type FromRef = BuilderRef;
    type ToRef = StorageRef;

    fn convert_ref<Target: 'ext>(
        &self,
        from_ref: <Self::FromRef as RefType<'ext>>::Node<Target>,
    ) -> <Self::ToRef as RefType<'ext>>::Node<Target> {
        let rel_pos = self
            .new_storage_pos
            .checked_sub(from_ref.abs_pos)
            .unwrap_or_else(|| panic!(""));

        StorageRef {
            rel_pos,
            phantom: PhantomData,
        }
    }
}

struct StorageToVisiting<'view, Container> {
    view: &'view [Container],
}
impl<'ext: 'view, 'view, Container> RefConverter<'ext> for StorageToVisiting<'view, Container> {
    type FromRef = StorageRef;
    type ToRef = VisitingRef<'view, Container>;

    fn convert_ref<Target: 'ext>(
        &self,
        from_ref: <Self::FromRef as RefType<'ext>>::Node<Target>,
    ) -> <Self::ToRef as RefType<'ext>>::Node<Target> {
        let index = self
            .view
            .len()
            .checked_sub(from_ref.rel_pos)
            .unwrap_or_else(|| panic!(""));
        VisitingRef {
            view: &self.view[..index],
            phantom: PhantomData,
        }
    }
}

pub trait HasDefaultContainer {
    type Container;
}

pub trait RecursiveFamily<'ext>: 'ext {
    type Sibling<R: RefType<'ext>>: RecursiveObj<'ext, Family = Self, Ref = R>;

    fn convert<FromRef, ToRef, Converter>(
        from_obj: Self::Sibling<FromRef>,
        converter: Converter,
    ) -> Self::Sibling<ToRef>
    where
        Self: Sized,
        Converter: RefConverter<'ext, FromRef = FromRef, ToRef = ToRef>,
        FromRef: RefType<'ext, ValueRef = ValueOwner>,
        ToRef: RefType<'ext, ValueRef = ValueOwner>;

    fn view<'view, FromRef, ToRef, Converter>(
        from_obj: &'view Self::Sibling<FromRef>,
        converter: Converter,
    ) -> Self::Sibling<ToRef>
    where
        Self: Sized,
        'ext: 'view,
        Converter: RefConverter<'ext, FromRef = FromRef, ToRef = ToRef>,
        FromRef: RefType<'ext, ValueRef = ValueOwner>,
        ToRef: RefType<'ext, ValueRef = ValueVisitor<'view>>;
}

pub trait RecursiveObj<'ext> {
    type Family: RecursiveFamily<'ext, Sibling<Self::Ref> = Self>;
    type Ref: RefType<'ext>;
}

pub trait VisitorOf<'ext, Target: RecursiveFamily<'ext>>: RefType<'ext> {
    type Error: Debug;
    fn try_borrow(self) -> Result<Target::Sibling<Self>, Self::Error>;
}
impl<'ext: 'view, 'view, Container, Target> VisitorOf<'ext, Target>
    for VisitingRef<'view, Container>
where
    Target: RecursiveFamily<'ext>,
    Container: TryAsRef<Target::Sibling<StorageRef>>,
    Container: From<Target::Sibling<StorageRef>>,
{
    type Error = ();
    fn try_borrow(self) -> Result<Target::Sibling<Self>, Self::Error> {
        let container = self.view.last().unwrap();
        let storage_obj = container.try_as_ref().unwrap();
        let converter = StorageToVisiting { view: self.view };
        let visiting_obj = Target::view(storage_obj, converter);
        Ok(visiting_obj)
    }
}

pub trait Visitable<'ext>: TypedNodeRef<'ext> {
    type Error: Debug;

    fn try_borrow(
        self,
    ) -> Result<<Self::Target as RecursiveFamily<'ext>>::Sibling<Self::Untyped>, Self::Error>
    where
        Self::Target: RecursiveFamily<'ext>;

    fn borrow(self) -> <Self::Target as RecursiveFamily<'ext>>::Sibling<Self::Untyped>
    where
        Self: Sized,
        Self::Target: RecursiveFamily<'ext>,
    {
        self.try_borrow().unwrap()
    }
}
impl<'ext, 'view, Target: RecursiveFamily<'ext>, TypedRef: TypedNodeRef<'ext, Target = Target>>
    Visitable<'ext> for TypedRef
where
    TypedRef::Untyped: VisitorOf<'ext, Target>,
{
    type Error = <TypedRef::Untyped as VisitorOf<'ext, Target>>::Error;

    fn try_borrow(
        self,
    ) -> Result<<Self::Target as RecursiveFamily<'ext>>::Sibling<Self::Untyped>, Self::Error> {
        self.strip_type().try_borrow()
    }
}

impl<'view, Container, Target> VisitingRef<'view, Container, Target> {
    pub fn borrow<'ext: 'view>(self) -> Target::Sibling<VisitingRef<'view, Container>>
    where
        Target: RecursiveFamily<'ext>,
        VisitingRef<'view, Container>: VisitorOf<'ext, Target>,
    {
        self.strip_type().try_borrow().unwrap()
    }
}

pub struct Arena<Container, Target = ()> {
    items: Vec<Container>,
    phantom: PhantomData<Target>,
}

impl<Container> Arena<Container, ()> {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            phantom: PhantomData,
        }
    }

    pub fn push<'ext, Obj: RecursiveObj<'ext, Ref = BuilderRef>>(
        &mut self,
        builder_obj: Obj,
    ) -> BuilderRef<Obj::Family>
    where
        <Obj::Family as RecursiveFamily<'ext>>::Sibling<StorageRef>: Into<Container>,
    {
        let converter = BuilderToStorage {
            new_storage_pos: self.items.len(),
        };
        let abs_pos = self.items.len();
        let storage_obj = <Obj::Family as RecursiveFamily<'ext>>::convert(builder_obj, converter);
        let container: Container = storage_obj.into();
        self.items.push(container);
        BuilderRef {
            abs_pos,
            phantom: PhantomData,
        }
    }

    pub fn finalize<Target>(mut self, root_ref: BuilderRef<Target>) -> Arena<Container, Target> {
        self.items.truncate(root_ref.abs_pos + 1);
        Arena {
            items: self.items,
            phantom: PhantomData,
        }
    }
}

impl<Container, Target> Arena<Container, Target> {
    pub fn visit_by_ref<RefTarget>(
        &self,
        builder_ref: BuilderRef<RefTarget>,
    ) -> VisitingRef<Container, RefTarget> {
        let range = ..builder_ref.abs_pos + 1;
        let view = self.items.get(range).expect("Index out of bounds");
        VisitingRef {
            view,
            phantom: PhantomData,
        }
    }

    pub fn visit_root<'ext: 'view, 'view>(&'view self) -> VisitingRef<'view, Container, Target>
    where
        Target: RecursiveFamily<'ext>,
    {
        VisitingRef {
            view: &self.items,
            phantom: PhantomData,
        }
    }
}

impl<Target: HasDefaultContainer> Arena<Target::Container, Target> {
    pub fn build<Func>(func: Func) -> Self
    where
        Func: FnOnce(&mut Arena<Target::Container>) -> BuilderRef<Target>,
    {
        let mut arena = Arena::new();
        let root_ref = func(&mut arena);
        arena.finalize(root_ref)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    mod with_ref {
        use super::super::{
            HasDefaultContainer, RecursiveFamily, RecursiveObj, RefConverter, RefType, StorageRef,
            ValueOwner, ValueRefType, ValueVisitor, Visitable, VisitorOf,
        };

        pub enum IntExpr<'a, R: RefType<'a> = StorageRef> {
            Int(<R::ValueRef as ValueRefType<'a>>::Value<i64>),
            IntRef(<R::ValueRef as ValueRefType<'a>>::Value<&'a i64>),
            Add(R::Node<IntExpr<'a>>, R::Node<IntExpr<'a>>),
        }

        impl<'ext> HasDefaultContainer for IntExpr<'ext> {
            type Container = Self;
        }

        impl<'ext> RecursiveFamily<'ext> for IntExpr<'ext> {
            type Sibling<R: RefType<'ext>> = IntExpr<'ext, R>;

            fn convert<FromRef, ToRef, Converter>(
                from_obj: Self::Sibling<FromRef>,
                converter: Converter,
            ) -> Self::Sibling<ToRef>
            where
                Self: Sized,
                Converter: RefConverter<'ext, FromRef = FromRef, ToRef = ToRef>,
                FromRef: RefType<'ext, ValueRef = ValueOwner>,
                ToRef: RefType<'ext, ValueRef = ValueOwner>,
            {
                match from_obj {
                    IntExpr::Int(a) => IntExpr::Int(a),
                    IntExpr::IntRef(a) => IntExpr::IntRef(a),
                    IntExpr::Add(a, b) => {
                        IntExpr::Add(converter.convert_ref(a), converter.convert_ref(b))
                    }
                }
            }

            fn view<'view, FromRef, ToRef, Converter>(
                from_obj: &'view Self::Sibling<FromRef>,
                converter: Converter,
            ) -> Self::Sibling<ToRef>
            where
                Self: Sized,
                'ext: 'view,
                Converter: RefConverter<'ext, FromRef = FromRef, ToRef = ToRef>,
                FromRef: RefType<'ext, ValueRef = ValueOwner>,
                ToRef: RefType<'ext, ValueRef = ValueVisitor<'view>>,
            {
                match from_obj {
                    IntExpr::Int(a) => IntExpr::Int(a),
                    IntExpr::IntRef(a) => IntExpr::IntRef(a),
                    IntExpr::Add(a, b) => {
                        IntExpr::Add(converter.convert_ref(*a), converter.convert_ref(*b))
                    }
                }
            }
        }

        impl<'a, R: RefType<'a>> RecursiveObj<'a> for IntExpr<'a, R> {
            type Family = IntExpr<'a>;
            type Ref = R;
        }

        pub trait VisitorOfIntExpr<'ext>:
            VisitorOf<'ext, IntExpr<'ext>, ValueRef = ValueVisitor<'ext>>
        {
        }
        impl<'ext, R> VisitorOfIntExpr<'ext> for R where
            Self: VisitorOf<'ext, IntExpr<'ext>, ValueRef = ValueVisitor<'ext>>
        {
        }

        impl<'view, V: VisitorOfIntExpr<'view>> IntExpr<'view, V> {
            pub fn eval(&self) -> i64 {
                match self {
                    IntExpr::Int(a) => **a,
                    IntExpr::IntRef(a) => ***a,
                    IntExpr::Add(a, b) => a.borrow().eval() + b.borrow().eval(),
                }
            }
        }
    }

    mod without_ref {
        use super::super::{
            HasDefaultContainer, RecursiveFamily, RecursiveObj, RefConverter, RefType, StorageRef,
            ValueOwner, ValueRefType, ValueVisitor, Visitable, VisitorOf,
        };

        pub enum IntExpr<R: RefType<'static> = StorageRef> {
            Int(<R::ValueRef as ValueRefType<'static>>::Value<i64>),
            Add(R::Node<IntExpr>, R::Node<IntExpr>),
        }

        impl HasDefaultContainer for IntExpr {
            type Container = Self;
        }

        impl RecursiveFamily<'static> for IntExpr {
            type Sibling<R: RefType<'static>> = IntExpr<R>;

            fn convert<FromRef, ToRef, Converter>(
                from_obj: Self::Sibling<FromRef>,
                converter: Converter,
            ) -> Self::Sibling<ToRef>
            where
                Self: Sized,
                Converter: RefConverter<'static, FromRef = FromRef, ToRef = ToRef>,
                FromRef: RefType<'static, ValueRef = ValueOwner>,
                ToRef: RefType<'static, ValueRef = ValueOwner>,
            {
                match from_obj {
                    IntExpr::Int(a) => IntExpr::Int(a),
                    IntExpr::Add(a, b) => {
                        IntExpr::Add(converter.convert_ref(a), converter.convert_ref(b))
                    }
                }
            }

            fn view<'view, FromRef, ToRef, Converter>(
                from_obj: &'view Self::Sibling<FromRef>,
                converter: Converter,
            ) -> Self::Sibling<ToRef>
            where
                Self: Sized,
                Converter: RefConverter<'static, FromRef = FromRef, ToRef = ToRef>,
                FromRef: RefType<'static, ValueRef = ValueOwner>,
                ToRef: RefType<'static, ValueRef = ValueVisitor<'view>>,
            {
                match from_obj {
                    IntExpr::Int(a) => IntExpr::Int(a),
                    IntExpr::Add(a, b) => {
                        IntExpr::Add(converter.convert_ref(*a), converter.convert_ref(*b))
                    }
                }
            }
        }

        impl<R: RefType<'static>> RecursiveObj<'static> for IntExpr<R> {
            type Family = IntExpr;
            type Ref = R;
        }

        pub trait VisitorOfIntExpr<'view>:
            VisitorOf<'static, IntExpr, ValueRef = ValueVisitor<'view>>
        {
        }
        impl<'view, R> VisitorOfIntExpr<'view> for R where
            Self: VisitorOf<'static, IntExpr, ValueRef = ValueVisitor<'view>>
        {
        }

        impl<'view, V: VisitorOfIntExpr<'view>> IntExpr<V> {
            pub fn eval(&self) -> i64 {
                match self {
                    IntExpr::Int(a) => **a,
                    IntExpr::Add(a, b) => a.borrow().eval() + b.borrow().eval(),
                }
            }
        }
    }

    #[test]
    fn test_intexpr_with_ref_by_hand() {
        use with_ref::*;
        let x = 10;
        let lhs: IntExpr = IntExpr::Int(5);
        let rhs: IntExpr = IntExpr::IntRef(&x);
        let lhs_ref = StorageRef {
            rel_pos: 2,
            phantom: PhantomData,
        };
        let rhs_ref = StorageRef {
            rel_pos: 1,
            phantom: PhantomData,
        };
        let sum = IntExpr::Add(lhs_ref, rhs_ref);
        let data = vec![lhs, rhs, sum];

        let root_ref: VisitingRef<_, IntExpr> = VisitingRef {
            view: &data,
            phantom: PhantomData,
        };
        let expected_root_visiting: IntExpr<VisitingRef<_>> = IntExpr::Add(
            VisitingRef {
                view: &data[..1],
                phantom: PhantomData,
            },
            VisitingRef {
                view: &data[..2],
                phantom: PhantomData,
            },
        );
        let root_visiting: IntExpr<VisitingRef<_>> = root_ref.borrow();

        assert_eq!(root_visiting.eval(), 15);
        assert_eq!(expected_root_visiting.eval(), 15);
    }

    #[test]
    fn test_intexpr_with_ref_in_arena() {
        use with_ref::*;

        let x = 10;
        let mut arena: Arena<IntExpr> = Arena::new();

        let lhs = arena.push(IntExpr::Int(5));
        let rhs = arena.push(IntExpr::IntRef(&x));
        let sum = arena.push(IntExpr::Add(lhs, rhs));
        let root_ref = arena.visit_by_ref(sum);
        let root_visiting = root_ref.borrow();

        assert_eq!(root_visiting.eval(), 15);
    }

    #[test]
    fn test_intexpr_without_ref_by_hand_without_ref() {
        use without_ref::*;
        let lhs: IntExpr = IntExpr::Int(5);
        let rhs: IntExpr = IntExpr::Int(10);
        let lhs_ref = StorageRef {
            rel_pos: 2,
            phantom: PhantomData,
        };
        let rhs_ref = StorageRef {
            rel_pos: 1,
            phantom: PhantomData,
        };
        let sum = IntExpr::Add(lhs_ref, rhs_ref);
        let data = vec![lhs, rhs, sum];

        let root_ref: VisitingRef<_, IntExpr> = VisitingRef {
            view: &data,
            phantom: PhantomData,
        };
        let expected_root_visiting: IntExpr<VisitingRef<_>> = IntExpr::Add(
            VisitingRef {
                view: &data[..1],
                phantom: PhantomData,
            },
            VisitingRef {
                view: &data[..2],
                phantom: PhantomData,
            },
        );
        let root_visiting: IntExpr<VisitingRef<'_, _>> = root_ref.borrow();

        assert_eq!(root_visiting.eval(), 15);
        assert_eq!(expected_root_visiting.eval(), 15);
    }

    #[test]
    fn test_intexpr_without_ref_in_arena() {
        use without_ref::*;

        let mut arena: Arena<IntExpr> = Arena::new();
        let lhs = arena.push(IntExpr::Int(5));
        let rhs = arena.push(IntExpr::Int(10));
        let sum = arena.push(IntExpr::Add(lhs, rhs));

        let root_ref = arena.visit_by_ref(sum);
        let root_obj = root_ref.borrow();

        assert_eq!(root_obj.eval(), 15);
    }

    #[test]
    fn arena_builder_for_intexpr_without_ref() {
        use without_ref::*;

        let arena = Arena::build(|arena| {
            let lhs = arena.push(IntExpr::Int(5));
            let rhs = arena.push(IntExpr::Int(10));
            let sum = arena.push(IntExpr::Add(lhs, rhs));
            sum
        });
        let root_ref = arena.visit_root();
        let root_obj = root_ref.borrow();

        assert_eq!(root_obj.eval(), 15);
    }
}
