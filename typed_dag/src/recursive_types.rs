use std::marker::PhantomData;

/// A tag indicating how value types should be represented in a
/// recursive object.
///
/// If a DAG contains value types, they must have different
/// representations depending on the usage.  For example, an abstract
/// syntax tree may contain integer literals as type `i64`.  These
/// should be held internally as `i64`, but exposed as `&i64` when
/// traversing the DAG.
///
/// The lifetime `'ext` is a lower bound on the lifetime of the
/// `Target` parameter, and allow a DAG to reference externally-owned
/// values with a non-static lifetime.
///
/// A `ValueRefType` should never be constructed, and exists to allow
/// [`RecursiveFamily::view`] and [`RecursiveFamily::convert`] to
/// reason about any value types they may manipulate.  For example,
/// the bound `FromRef: RefType<'ext, Self, ValueRef = ValueOwner>`
/// used in [`RecursiveFamily::convert`] allows `<FromRef::ValueRef as
/// ValueRefType<'ext>>::Value<i64>` to be normalized to `i64`, based
/// on the known bound of `ValueRef = ValueOwner`.  Expressing a
/// similar constraint without the helper trait would require
/// [higher-ranked types in trait
/// bounds](https://github.com/rust-lang/rfcs/issues/1481).
pub trait ValueRefType<'ext> {
    type Value<Target: 'ext>;
}

/// A tag indicating that the object owns the leaf values.
///
/// The `ValueOwner` tag implements [ValueRefType] for all lifetimes.
/// That is, an object that owns its leafs values may contain leaf
/// values of any lifetime.
pub struct ValueOwner;

/// A tag indicating that the object holds views to its leaf values.
///
/// The `ValueVisitor<'view>` tag only implements
/// [`ValueRefType<'ext>`] when the lifetime `'ext` outlives `'view`.
/// That is, if an object holds references with a lifetime `'view`,
/// then the referred-to leaf values must outlive `'view`.
pub struct ValueVisitor<'view> {
    phantom: PhantomData<&'view ()>,
}

impl<'ext> ValueRefType<'ext> for ValueOwner {
    type Value<Target: 'ext> = Target;
}
impl<'ext: 'view, 'view> ValueRefType<'ext> for ValueVisitor<'view> {
    type Value<Target: 'ext> = &'view Target;
}

/// Abstract across a type of references
///
/// Allows the same generic enum definition to be used both as a value
/// type using `GraphRef<T>` instances for recursive references and as
/// a lifetimed type using `LiveGraphRef<'a, BaseType, T>` for
/// recursive references.
/// A tag indicating how recursive structs should be represented
///
/// This type is used as a generic parameter when defining a family of
/// related data structures.
///
/// For example, consider a structure defining integer expressions.
/// Each expression may either be an integer literal, or the sum of
/// two expressions of integers.  We could express this using the
/// following two data structures.
///
/// ```rust
/// enum IntExprWithBackref {
///     // Leaf value are held by value.
///     Int(i64),
///
///     // References to other integer expressions are represented
///     // as indices.
///     Add(usize, usize),
/// }
/// enum IntExprWithView<'view> {
///     // Leaf values are viewed as references.
///     Int(&'view i64),
///
///     // References to other integer expressions are represented
///     // as views into the contiguous slice that contains the
///     // sub-expression.
///     Add(&'view [IntExprWithBackref], &'view [IntExprWithBackref]),
/// }
/// ```
///
/// However, this requires a large amount of repetition between the
/// two data structures.  By defining the data structures as being
/// generic over some `RefType`, the two data structures can share a
/// single definition.
///
/// ```
/// # use typed_dag::{RefType, StorageRef, ValueRefType};
/// enum IntExpr<R: RefType<'static>> {
///     Int(<R::ValueRef as ValueRefType<'static>>::Value<i64>),
///     Add(R::Node<IntExpr<StorageRef>>, R::Node<IntExpr<StorageRef>>),
/// }
/// ```
///
/// With this single definition, the two earlier data structures can
/// now be written as `IntExpr<StorageRef>` and
/// `IntExpr<VisitingRef<'view, IntExpr<StorageRef>>>`, using the
/// [`StorageRef`] and [`VisitingRef`] tags.
///
/// The user-provided data structures can be further simplified using
/// the [`typed_dag`] macro, which also provides several trait
/// implementations required for full use of the DAG.
///
/// ```
/// # use typed_dag_derive::typed_dag;
/// #[typed_dag]
/// mod expr {
///     enum IntExpr {
///         Int(i64),
///         Add(IntExpr, IntExpr),
///     }
/// }
/// ```
pub trait RefType<'ext>: Sized + Copy + Clone {
    type ValueRef: ValueRefType<'ext>;
    type Node<Target: 'ext>: TypedNodeRef<'ext, Untyped = Self, Target = Target>;
}

/// A type-tagged reference to another node in the graph.
///
/// Each `TypedNodeRef` has the same in-memory representation as the
/// corresponding [`RefType`], with a compile-time tag indicating the
/// type to which it points.
pub trait TypedNodeRef<'ext>: Copy + Clone {
    /// The corresponding [`RefType`]
    type Untyped: RefType<'ext>;

    /// The pointed-to object type.
    type Target: 'ext;

    /// Remove the type annotation.  This is used internally, and
    /// should generally not be called by a user.
    fn strip_type(&self) -> Self::Untyped;
}

/// Defines a utility for converting between reference types
///
/// Provided to the [`RecursiveFamily`] conversion methods, when
/// converting between different variants of the same generic
/// user-defined data structure.
pub trait RefConverter<'ext> {
    /// The reference type being converted from
    type FromRef: RefType<'ext>;
    type ToRef: RefType<'ext>;

    fn convert_ref<Target: 'ext>(
        &self,
        from_ref: <Self::FromRef as RefType<'ext>>::Node<Target>,
    ) -> <Self::ToRef as RefType<'ext>>::Node<Target>;
}

/// A type suitable for use as a container of this recursive type.
/// Should implement From<Target> and TryAsRef<Target> for all
/// recursive nodes that may appear within the type, either
/// directly or indirectly.
///
/// Functionality should not be constrained based on the type of the
/// container, which should only be used as an overrideable default.
pub trait HasDefaultContainer {
    type Container;
}

/// A user-defined data structure for a node in a DAG
///
/// The representation of all nodes in `typed_dag`.  The lifetime
/// `'ext` indicates a minimum lifetime of external lifetimes, and
/// should be `'static` if no such references exist.
pub trait RecursiveFamily<'ext>: 'ext {
    type Sibling<R: RefType<'ext>>: RecursiveObj<'ext, Family = Self, Ref = R>;

    /// Convert between two objects, each of which own their leaf nodes
    fn convert<FromRef, ToRef, Converter>(
        from_obj: Self::Sibling<FromRef>,
        converter: Converter,
    ) -> Self::Sibling<ToRef>
    where
        Self: Sized,
        Converter: RefConverter<'ext, FromRef = FromRef, ToRef = ToRef>,
        FromRef: RefType<'ext, ValueRef = ValueOwner>,
        ToRef: RefType<'ext, ValueRef = ValueOwner>;

    /// Convert from an object that owns its leaf nodes, into one that
    /// views its leaf nodes.
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

/// An instance of a user-defined object
///
/// Used to define methods that require a specific reference type
/// (e.g. [`Arena::push`]).
pub trait RecursiveObj<'ext> {
    /// The family to which this object belongs.
    type Family: RecursiveFamily<'ext, Sibling<Self::Ref> = Self>;

    /// The reference type being used.
    type Ref: RefType<'ext>;
}
