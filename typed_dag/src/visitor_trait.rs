use crate::{
    Error, RecursiveFamily, RefType, StorageRef, StorageToVisiting, TryAsRef, TypedNodeRef,
    VisitingRef,
};
use std::fmt::Debug;

/// A reference type that may be visited
///
/// This is a subtrait of [`RefType`], and can be used as the generic
/// parameter of a recursive object.  Any reference to `Target` within
/// that recursive object can be recursively visited.
pub trait VisitorOf<'ext, Target: RecursiveFamily<'ext>>: RefType<'ext> {
    type Error: Debug;

    /// Attempt to visit the referenced type
    ///
    /// For a well-formed DAG, this should never fail.  Failure can
    /// occur due to a relative index being out of range, or due to a
    /// pointed-to object having an unexpected type.
    fn try_expand(typed_ref: Self::Node<Target>) -> Result<Target::Sibling<Self>, Self::Error>;
}
impl<'ext: 'view, 'view, Container, Target> VisitorOf<'ext, Target>
    for VisitingRef<'view, Container>
where
    Target: RecursiveFamily<'ext>,
    Container: TryAsRef<Target::Sibling<StorageRef>, Error = Error>,
    Container: From<Target::Sibling<StorageRef>>,
{
    type Error = Error;

    fn try_expand(typed_ref: Self::Node<Target>) -> Result<Target::Sibling<Self>, Self::Error> {
        let container = typed_ref.view.last().ok_or(Error::EmptyExpression)?;
        let storage_obj = container.try_as_ref()?;
        let converter = StorageToVisiting {
            view: typed_ref.view,
        };
        let visiting_obj = Target::view(storage_obj, converter);
        Ok(visiting_obj)
    }
}

/// Defines a visitable typed reference
pub trait Visitable<'ext>: TypedNodeRef<'ext> {
    type Error: Debug;

    fn try_expand(
        self,
    ) -> Result<<Self::Target as RecursiveFamily<'ext>>::Sibling<Self::Untyped>, Self::Error>
    where
        Self::Target: RecursiveFamily<'ext>;

    fn expand(self) -> <Self::Target as RecursiveFamily<'ext>>::Sibling<Self::Untyped>
    where
        Self: Sized,
        Self::Target: RecursiveFamily<'ext>,
        Self::Error: Debug,
    {
        self.try_expand().unwrap()
    }
}
impl<
        'ext,
        'view,
        Target: RecursiveFamily<'ext>,
        Untyped,
        TypedRef: TypedNodeRef<'ext, Target = Target, Untyped = Untyped>,
    > Visitable<'ext> for TypedRef
where
    Untyped: VisitorOf<'ext, Target, Node<Target> = Self>,
{
    type Error = <TypedRef::Untyped as VisitorOf<'ext, Target>>::Error;

    fn try_expand(
        self,
    ) -> Result<<Self::Target as RecursiveFamily<'ext>>::Sibling<Self::Untyped>, Self::Error> {
        <Untyped as VisitorOf<'ext, Target>>::try_expand(self)
    }
}
