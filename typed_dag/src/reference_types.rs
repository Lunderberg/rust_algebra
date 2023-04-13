use std::marker::PhantomData;

use crate::{RefType, TypedNodeRef, ValueOwner, ValueVisitor};

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
