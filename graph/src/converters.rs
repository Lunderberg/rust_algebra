use std::marker::PhantomData;

use crate::{
    Builder, BuilderRef, RefTypeMover, RefTypeViewer, Storage, StorageRef, Visiting, VisitingRef,
};

/// Converts from Builder references using absolute position to
/// Storage references using relative position.
pub struct BuilderToStorage {
    pub(crate) size: usize,
}

impl BuilderToStorage {
    pub fn move_reference<T>(&self, old_ref: BuilderRef<T>) -> StorageRef<T> {
        let rel_pos = self
            .size
            .checked_sub(old_ref.abs_pos)
            .expect("Invalid reference type");
        StorageRef {
            rel_pos,
            _node: PhantomData,
        }
    }
}

impl<'a> RefTypeMover<'a, Builder, Storage> for BuilderToStorage {
    fn move_reference<T>(&self, old_ref: BuilderRef<T>) -> StorageRef<T> {
        self.move_reference(old_ref)
    }

    fn move_value<T>(&self, value: T) -> T {
        value
    }
}

/// Converts from Storage references as they are stored in the
/// contiguous array to a Visiting reference
pub struct StorageToVisiting<'a, Container> {
    pub(crate) view: &'a [Container],
}

impl<'a, Container> StorageToVisiting<'a, Container> {
    pub fn view_reference<T>(&self, old_ref: &StorageRef<T>) -> VisitingRef<'a, T, Container> {
        VisitingRef {
            rel_pos: old_ref.rel_pos,
            view: self.view,
            _phantom: PhantomData,
        }
    }
}

impl<'a, Container> RefTypeViewer<'a, Storage, Visiting<'a, Container>>
    for StorageToVisiting<'a, Container>
{
    fn view_reference<T>(&self, old_ref: &StorageRef<T>) -> VisitingRef<'a, T, Container> {
        self.view_reference(old_ref)
    }

    fn view_value<T>(&self, value: &'a T) -> &'a T {
        value
    }
}
