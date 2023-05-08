use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

use crate::{
    NodeRefType, RecursiveFamily, RecursiveObj, TypedNodeRef, ValueOwner, ValueRefType,
    ValueVisitor,
};

/// A reference to an object, returned from [`Arena::push`]
///
/// The lack of cycles is guaranteed by contruction, based on the
/// following three rules.
///
/// 1. Each `BuilderRef` is the result of adding an object to the
/// arena, returned by [`Arena::push`].
///
/// 2. Each new object must express references using `BuilderRef`.
///
/// 3. Objects in the [`Arena`] may not be modified.
pub struct AbsolutePos<Target = ()> {
    pub(crate) abs_pos: usize,
    pub(crate) phantom: PhantomData<Target>,
}

impl<Target> Clone for AbsolutePos<Target> {
    fn clone(&self) -> Self {
        Self {
            abs_pos: self.abs_pos,
            phantom: PhantomData,
        }
    }
}
impl<Target> Copy for AbsolutePos<Target> {}
impl<Target> Debug for AbsolutePos<Target> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuilderRef")
            .field("abs_pos", &self.abs_pos)
            .field("phantom", &self.phantom)
            .finish()
    }
}
impl<'ext> NodeRefType<'ext> for AbsolutePos {
    type Node<Target: RecursiveFamily<'ext>> = AbsolutePos<Target>;
    type DefaultValueRef = ValueOwner;
}

impl<'ext, Target: RecursiveFamily<'ext>> TypedNodeRef<'ext> for AbsolutePos<Target> {
    type Untyped = AbsolutePos;
    type Target = Target;
}

/// A reference to an object, stored internally in [`Arena`]
///
/// The lack of cycles is guaranteed by construction.  Each
/// `StorageRef` may only point to objects that are located earlier in
/// the [`Arena`]'s internal storage.  Since following a reference
/// must result in a strictly smaller index within the internal
/// storage, no reference may lead back to the original node.
pub struct RelativePos<Target = ()> {
    pub(crate) rel_pos: usize,
    pub(crate) phantom: PhantomData<Target>,
}
impl<Target> Clone for RelativePos<Target> {
    fn clone(&self) -> Self {
        RelativePos {
            rel_pos: self.rel_pos,
            phantom: PhantomData,
        }
    }
}
impl<Target> Copy for RelativePos<Target> {}
impl<Target> Debug for RelativePos<Target> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StorageRef")
            .field("rel_pos", &self.rel_pos)
            .field("phantom", &self.phantom)
            .finish()
    }
}

impl<'ext> NodeRefType<'ext> for RelativePos {
    type Node<Target: RecursiveFamily<'ext>> = RelativePos<Target>;
    type DefaultValueRef = ValueOwner;
}
impl<'ext, Target: RecursiveFamily<'ext>> TypedNodeRef<'ext> for RelativePos<Target> {
    type Untyped = RelativePos;
    type Target = Target;
}

/// A reference to an object, exposed while traversing a tree
///
/// Each `VisitingRef` contains a reference to a slice of contiguous
/// memory.  The last element of the slice is the referred-to object,
/// and the slice contains all objects that may be referenced,
/// directly or indirectly, by the referred-to object.  Following a
/// reference requires constructing a new `VisitingRef`, which holds a
/// subslice of the original view.  Since the slice of a sub-object
/// must be strictly smaller than an object's slice, no reference can
/// return the original object, making cycles unrepresentable.
pub struct View<'view, Container, Target = ()> {
    pub(crate) view: &'view [Container],
    pub(crate) phantom: PhantomData<&'view Target>,
}
impl<'view, Container, Target> Clone for View<'view, Container, Target> {
    fn clone(&self) -> Self {
        Self {
            view: self.view,
            phantom: PhantomData,
        }
    }
}
impl<'view, Container, Target> Copy for View<'view, Container, Target> {}
impl<'view, Container, Target> Debug for View<'view, Container, Target> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VisitingRef")
            .field("view", &"view")
            .field("phantom", &self.phantom)
            .finish()
    }
}

impl<'ext: 'view, 'view, Container> NodeRefType<'ext> for View<'view, Container> {
    type Node<Target: RecursiveFamily<'ext>> = View<'view, Container, Target>;
    type DefaultValueRef = ValueVisitor<'view>;
}
impl<'ext: 'view, 'view, Container, Target: RecursiveFamily<'ext>> TypedNodeRef<'ext>
    for View<'view, Container, Target>
{
    type Untyped = View<'view, Container>;
    type Target = Target;
}

pub struct NestedVisitor<N, V, Target = ()> {
    phantom: PhantomData<(N, V, Target)>,
}
impl<'ext, N: NodeRefType<'ext>, V: ValueRefType<'ext>> NodeRefType<'ext> for NestedVisitor<N, V> {
    type Node<Target: RecursiveFamily<'ext>> = Target::Sibling<N, V>;
    type DefaultValueRef = N::DefaultValueRef;
}
impl<'ext, Obj: RecursiveObj<'ext>> TypedNodeRef<'ext> for Obj {
    type Untyped = NestedVisitor<Obj::NodeRef, Obj::ValueRef>;
    type Target = Obj::Family;
}
