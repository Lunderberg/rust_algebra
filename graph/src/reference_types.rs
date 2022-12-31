/// Abstract across a type of references
///
/// Allows the same generic enum definition to be used both as a value
/// type using `GraphRef<T>` instances for recursive references and as
/// a lifetimed type using `LiveGraphRef<'a, BaseType, T>` for
/// recursive references.
pub trait RecursiveRefType<'a>: 'a {
    /// The representation to use for recursive references
    ///
    /// # Arguments
    ///
    /// `T` - The type to which the reference points
    ///
    /// # Returns
    ///
    /// The representation of a recursive reference to `T` for
    /// this reference type.
    type Ref<T: ?Sized>;

    /// The representation to use for all other types
    ///
    /// # Arguments
    ///
    /// `T` - The value type to represent
    ///
    /// # Returns
    ///
    /// The representation of a value type `T` for this usage.
    /// (e.g. `T` for storage, `&T` for visiting)
    type Value<T: 'a>;
}

/// Meta-object, at compile-time can generate an enum for a
/// specific usage type.  At runtime, can convert between enums
/// of different usage types.
pub trait RecursiveFamily {
    type Obj<'a, R: RecursiveRefType<'a>>: RecursiveObj<'a, RefType = R, Family = Self> + 'a;

    type DefaultContainer<'a>: 'a;

    fn view_ref<'a, OldRef: RecursiveRefType<'a>, NewRef: RecursiveRefType<'a>>(
        old_obj: &'a Self::Obj<'a, OldRef>,
        viewer: &impl RefTypeViewer<'a, OldRef, NewRef>,
    ) -> Self::Obj<'a, NewRef>;

    fn move_ref<'a, OldRef: RecursiveRefType<'a>, NewRef: RecursiveRefType<'a>>(
        old_obj: Self::Obj<'a, OldRef>,
        viewer: &impl RefTypeMover<'a, OldRef, NewRef>,
    ) -> Self::Obj<'a, NewRef>;
}

/// A recursive object, belonging to a specific family of
/// recursive types, with a specific type in that family.  This
/// trait is automatically implemented, and is used for functions
/// that must derive their types from arguments, and return
/// another type in the same family.  (e.g. `Builder::push`
/// accepts an argument of type `F::Obj<Builder>` and must
/// internally convert it to an object of type `F::Obj<Storage>`)
pub trait RecursiveObj<'a>: 'a {
    type Family: RecursiveFamily;
    type RefType: RecursiveRefType<'a>;
}

/// Convert between references of different usage types
/// (e.g. build a Visitor enum from a Storage enum)
pub trait RefTypeViewer<'a, OldRef: RecursiveRefType<'a>, NewRef: RecursiveRefType<'a>>:
    'a
{
    fn view_reference<T>(&self, old_ref: &OldRef::Ref<T>) -> NewRef::Ref<T>;
    fn view_value<T>(&self, value: &'a OldRef::Value<T>) -> NewRef::Value<T>;
}

/// Convert between references of different usage types
/// (e.g. build a Storage enum from a Builder enum)
pub trait RefTypeMover<'a, OldRef: RecursiveRefType<'a>, NewRef: RecursiveRefType<'a>>: 'a {
    fn move_reference<T>(&self, old_ref: OldRef::Ref<T>) -> NewRef::Ref<T>;
    fn move_value<T>(&self, value: OldRef::Value<T>) -> NewRef::Value<T>;
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
    type Value<T: 'a> = ();
}
