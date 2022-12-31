use std::marker::PhantomData;

use crate::{
    Builder, BuilderRef, RefTypeMover, RefTypeViewer, Storage, StorageRef, Visiting, VisitingRef,
};

/// Converts from Builder references using absolute position to
/// Storage references using relative position.
pub struct BuilderToStorage {
    pub(crate) size: usize,
}

impl<'a, Container: 'a> RefTypeMover<'a, Builder<Container>, Storage> for BuilderToStorage {
    fn move_reference<T>(&self, old_ref: BuilderRef<T>) -> StorageRef<T> {
        let rel_pos = self
            .size
            .checked_sub(old_ref.abs_pos)
            .expect("Invalid reference type");
        StorageRef {
            rel_pos,
            _node: PhantomData,
        }
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

impl<'a, Container> RefTypeViewer<'a, Storage, Visiting<'a, Container>>
    for StorageToVisiting<'a, Container>
{
    fn view_reference<T>(&self, old_ref: &StorageRef<T>) -> VisitingRef<'a, T, Container> {
        VisitingRef {
            rel_pos: old_ref.rel_pos,
            view: self.view,
            _phantom: PhantomData,
        }
    }

    fn view_value<T>(&self, value: &'a T) -> &'a T {
        value
    }
}
