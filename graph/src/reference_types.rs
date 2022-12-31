use crate::{Builder, BuilderToStorage, Storage, StorageToVisiting, Visiting};

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
    type RefType: RecursiveRefType<'a>;
}
