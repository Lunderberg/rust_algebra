use std::fmt::{Debug, Formatter};
use std::marker::PhantomData;

use crate::{RecursiveFamily, RecursiveObj, RefType, TypedNodeRef, ValueOwner, ValueVisitor};

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
pub struct BuilderRef<Target = ()> {
    pub(crate) abs_pos: usize,
    pub(crate) phantom: PhantomData<Target>,
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
impl<Target> Debug for BuilderRef<Target> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuilderRef")
            .field("abs_pos", &self.abs_pos)
            .field("phantom", &self.phantom)
            .finish()
    }
}
impl<'ext> RefType<'ext> for BuilderRef {
    type ValueRef = ValueOwner;
    type Node<Target: RecursiveFamily<'ext>> = BuilderRef<Target>;
}

impl<'ext, Target: RecursiveFamily<'ext>> TypedNodeRef<'ext> for BuilderRef<Target> {
    type Untyped = BuilderRef;
    type Target = Target;
}

/// A reference to an object, stored internally in [`Arena`]
///
/// The lack of cycles is guaranteed by construction.  Each
/// `StorageRef` may only point to objects that are located earlier in
/// the [`Arena`]'s internal storage.  Since following a reference
/// must result in a strictly smaller index within the internal
/// storage, no reference may lead back to the original node.
pub struct StorageRef<Target = ()> {
    pub(crate) rel_pos: usize,
    pub(crate) phantom: PhantomData<Target>,
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
impl<Target> Debug for StorageRef<Target> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StorageRef")
            .field("rel_pos", &self.rel_pos)
            .field("phantom", &self.phantom)
            .finish()
    }
}

impl<'ext> RefType<'ext> for StorageRef {
    type ValueRef = ValueOwner;
    type Node<Target: RecursiveFamily<'ext>> = StorageRef<Target>;
}
impl<'ext, Target: RecursiveFamily<'ext>> TypedNodeRef<'ext> for StorageRef<Target> {
    type Untyped = StorageRef;
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
pub struct VisitingRef<'view, Container, Target = ()> {
    pub(crate) view: &'view [Container],
    pub(crate) phantom: PhantomData<&'view Target>,
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
impl<'view, Container, Target> Debug for VisitingRef<'view, Container, Target> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VisitingRef")
            .field("view", &"view")
            .field("phantom", &self.phantom)
            .finish()
    }
}

impl<'ext: 'view, 'view, Container> RefType<'ext> for VisitingRef<'view, Container> {
    type ValueRef = ValueVisitor<'view>;
    type Node<Target: RecursiveFamily<'ext>> = VisitingRef<'view, Container, Target>;
}
impl<'ext: 'view, 'view, Container, Target: RecursiveFamily<'ext>> TypedNodeRef<'ext>
    for VisitingRef<'view, Container, Target>
{
    type Untyped = VisitingRef<'view, Container>;
    type Target = Target;
}

pub struct NestedVisitor<'view, R, Target = ()> {
    _inner: R,
    phantom: PhantomData<&'view Target>,
}
impl<'ext: 'view, 'view, R: RefType<'ext, ValueRef = ValueVisitor<'view>>> RefType<'ext>
    for NestedVisitor<'view, R>
{
    type ValueRef = ValueVisitor<'view>;
    type Node<Target: RecursiveFamily<'ext>> = Target::Sibling<R>;
}
impl<
        'ext: 'view,
        'view,
        R: RefType<'ext, ValueRef = ValueVisitor<'view>>,
        Obj: RecursiveObj<'ext, Ref = R>,
    > TypedNodeRef<'ext> for Obj
{
    type Untyped = NestedVisitor<'view, R>;
    type Target = Obj::Family;
}
