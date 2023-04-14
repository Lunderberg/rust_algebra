use crate::{
    RecursiveFamily, RefType, StorageRef, StorageToVisiting, TryAsRef, TypedNodeRef, VisitingRef,
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
    fn try_borrow(self) -> Result<Target::Sibling<Self>, Self::Error>;
}
impl<'ext: 'view, 'view, Container, Target> VisitorOf<'ext, Target>
    for VisitingRef<'view, Container>
where
    Target: RecursiveFamily<'ext>,
    Container: TryAsRef<Target::Sibling<StorageRef>>,
    Container: From<Target::Sibling<StorageRef>>,
{
    // TODO: Actual error type
    type Error = ();
    fn try_borrow(self) -> Result<Target::Sibling<Self>, Self::Error> {
        let container = self.view.last().unwrap();
        let storage_obj = container.try_as_ref().unwrap();
        let converter = StorageToVisiting { view: self.view };
        let visiting_obj = Target::view(storage_obj, converter);
        Ok(visiting_obj)
    }
}

/// Defines a visitable typed reference
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
        Self::Error: Debug,
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

// impl<'view, Container, Target> VisitingRef<'view, Container, Target> {
//     pub fn borrow<'ext: 'view, Error>(self) -> Target::Sibling<VisitingRef<'view, Container>>
//     where
//         Target: RecursiveFamily<'ext>,
//         VisitingRef<'view, Container>: VisitorOf<'ext, Target>,
//         <VisitingRef<'view, Container> as VisitorOf<'ext, Target>>::Error: Debug,
//     {
//         self.strip_type().try_borrow().unwrap()
//     }
// }