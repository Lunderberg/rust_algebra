use crate::{AbsolutePos, Error, NodeRefType, RecursiveFamily, RefConverter, RelativePos, View};
use std::marker::PhantomData;

/// Convert from a BuilderRef to StorageRef
///
/// The [`BuilderRef`] contains absolute indices, while the
/// [`StorageRef`] contains relative indices.  This converter uses the
/// position of the newly created [`RecursiveObj`]`<Ref = StorageRef>`
/// contained in the [`Arena`] to generate the appropriate relative
/// indices.
pub(crate) struct BuilderToStorage {
    pub(crate) new_storage_pos: usize,
}
impl<'ext> RefConverter<'ext> for BuilderToStorage {
    type FromRef = AbsolutePos;
    type ToRef = RelativePos;

    fn convert_ref<Target: RecursiveFamily<'ext>>(
        &self,
        from_ref: &<Self::FromRef as NodeRefType<'ext>>::Node<Target>,
    ) -> <Self::ToRef as NodeRefType<'ext>>::Node<Target> {
        let rel_pos = self
            .new_storage_pos
            .checked_sub(from_ref.abs_pos)
            .ok_or_else(|| Error::InvalidAbsoluteReference {
                abs_pos: from_ref.abs_pos,
                subgraph_size: self.new_storage_pos,
            })
            .unwrap();

        RelativePos {
            rel_pos,
            phantom: PhantomData,
        }
    }
}

/// Convert from a StorageRef to VisitingRef
///
/// The [`StorageRef`] contains relative indices, while the
/// [`VisitingRef`] contains a view to the objects themselves.  This
/// converter uses the slice whose top element is the object holding
/// the [`StorageRef`] to generate the appropriate sub-slices for use
/// in a [`VisitingRef`].
pub(crate) struct StorageToVisiting<'view, Container> {
    pub(crate) view: &'view [Container],
}
impl<'ext: 'view, 'view, Container> RefConverter<'ext> for StorageToVisiting<'view, Container> {
    type FromRef = RelativePos;
    type ToRef = View<'view, Container>;

    fn convert_ref<Target: RecursiveFamily<'ext>>(
        &self,
        from_ref: &<Self::FromRef as NodeRefType<'ext>>::Node<Target>,
    ) -> <Self::ToRef as NodeRefType<'ext>>::Node<Target> {
        let index = self
            .view
            .len()
            .checked_sub(from_ref.rel_pos)
            .ok_or_else(|| Error::InvalidRelativeReference {
                rel_pos: from_ref.rel_pos,
                subgraph_size: self.view.len(),
            })
            .unwrap();
        View {
            view: &self.view[..index],
            phantom: PhantomData,
        }
    }
}
