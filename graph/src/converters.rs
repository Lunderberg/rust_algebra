use std::marker::PhantomData;

use crate::{BuilderRef, StorageRef, VisitingRef};

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
