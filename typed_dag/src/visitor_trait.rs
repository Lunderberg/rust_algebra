use crate::{
    Error, NodeRefType, RecursiveFamily, RecursiveObj, RelativePos, StorageToVisiting, TryAsRef,
    TypedNodeRef, ValueOwner, ValueRefType, ValueVisitor, View,
};
use std::fmt::Debug;

/// A reference type that may be visited
///
/// This is a subtrait of [`RefType`], and can be used as the generic
/// parameter of a recursive object.  Any reference to `Target` within
/// that recursive object can be recursively visited.
pub trait VisitorOf<'ext, Target: RecursiveFamily<'ext>>: Sized + NodeRefType<'ext> {
    type Error: Debug;

    type ValueRef: ValueRefType<'ext>;

    /// Attempt to visit the referenced type
    ///
    /// For a well-formed DAG, this should never fail.  Failure can
    /// occur due to a relative index being out of range, or due to a
    /// pointed-to object having an unexpected type.
    fn try_expand_impl(
        typed_ref: &Self::Node<Target>,
    ) -> Result<Target::Sibling<Self, Self::ValueRef>, Self::Error>;
}
impl<'ext: 'view, 'view, Container, Target> VisitorOf<'ext, Target> for View<'view, Container>
where
    Target: RecursiveFamily<'ext>,
    Container: TryAsRef<Target::Sibling<RelativePos, ValueOwner>, Error = Error>,
    Container: From<Target::Sibling<RelativePos, ValueOwner>>,
{
    type Error = Error;
    type ValueRef = ValueVisitor<'view>;

    fn try_expand_impl(
        typed_ref: &<Self as NodeRefType<'ext>>::Node<Target>,
    ) -> Result<Target::Sibling<Self, Self::ValueRef>, Self::Error> {
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

    type NodeRef: NodeRefType<'ext>;
    type ValueRef: ValueRefType<'ext>;
    type Output: RecursiveObj<
        'ext,
        Family = Self::Target,
        NodeRef = Self::NodeRef,
        ValueRef = Self::ValueRef,
    >;

    fn try_expand(&self) -> Result<Self::Output, Self::Error>
    where
        Self::Target: RecursiveFamily<'ext>;

    fn expand(&self) -> Self::Output
    where
        Self: Sized,
        Self::Target: RecursiveFamily<'ext>,
        Self::Error: Debug,
    {
        self.try_expand().unwrap()
    }
}
impl<'ext, TypedRef, NodeRef, Target> Visitable<'ext> for TypedRef
where
    Target: RecursiveFamily<'ext>,
    TypedRef: TypedNodeRef<'ext, Untyped = NodeRef, Target = Target>,
    NodeRef: VisitorOf<'ext, Target, Node<Target> = TypedRef>,
{
    type Error = <NodeRef as VisitorOf<'ext, Target>>::Error;
    type NodeRef = NodeRef;
    type ValueRef = <NodeRef as VisitorOf<'ext, Target>>::ValueRef;
    type Output = Target::Sibling<NodeRef, Self::ValueRef>;

    fn try_expand(&self) -> Result<Self::Output, Self::Error>
    where
        Self::Target: RecursiveFamily<'ext>,
    {
        NodeRef::try_expand_impl(self)
    }
}
