use crate::{
    AbsolutePos, BuilderToStorage, HasDefaultContainer, RecursiveFamily, RecursiveObj, RelativePos,
    ValueOwner, View,
};
use std::marker::PhantomData;

/// An Arena whose elements may be
///
/// An untyped arena (`Target = ()`) may have additional items
/// inserted.  A typed arena is tagged with a single `Target` at the
/// root of the graph, but may no longer be modified.
pub struct Arena<Container, Target = ()> {
    items: Vec<Container>,
    phantom: PhantomData<Target>,
}

impl<Container> Arena<Container, ()> {
    pub fn new() -> Self {
        Self {
            items: Vec::new(),
            phantom: PhantomData,
        }
    }

    pub fn push<'ext, Obj: RecursiveObj<'ext, NodeRef = AbsolutePos, ValueRef = ValueOwner>>(
        &mut self,
        builder_obj: Obj,
    ) -> AbsolutePos<Obj::Family>
    where
        <Obj::Family as RecursiveFamily<'ext>>::Sibling<RelativePos, ValueOwner>: Into<Container>,
    {
        let converter = BuilderToStorage {
            new_storage_pos: self.items.len(),
        };
        let abs_pos = self.items.len();
        let storage_obj = <Obj::Family as RecursiveFamily<'ext>>::convert(builder_obj, converter);
        let container: Container = storage_obj.into();
        self.items.push(container);
        AbsolutePos {
            abs_pos,
            phantom: PhantomData,
        }
    }

    pub fn finalize<Target>(mut self, root_ref: AbsolutePos<Target>) -> Arena<Container, Target> {
        // References may only point to earlier elements, so anything
        // after the root ref may be discarded.  This also avoids
        // needing to store the location of the root object in the
        // finalized output.
        self.items.truncate(root_ref.abs_pos + 1);
        Arena {
            items: self.items,
            phantom: PhantomData,
        }
    }
}

impl<Container, Target> Arena<Container, Target> {
    pub fn visit_by_ref<RefTarget>(
        &self,
        builder_ref: AbsolutePos<RefTarget>,
    ) -> View<Container, RefTarget> {
        let range = ..builder_ref.abs_pos + 1;
        let view = self.items.get(range).expect("Index out of bounds");
        View {
            view,
            phantom: PhantomData,
        }
    }

    /// Return a visiting reference to the root node.  This is only
    /// available for finalized objects, since it requires the type of
    /// the root node is known.
    pub fn visit_root<'ext: 'view, 'view>(&'view self) -> View<'view, Container, Target>
    where
        Target: RecursiveFamily<'ext>,
    {
        View {
            view: &self.items,
            phantom: PhantomData,
        }
    }
}

impl<Target: HasDefaultContainer> Arena<Target::Container, Target> {
    /// Utility function to construct an expression with a default
    /// container.
    ///
    /// If the type of the container should be different than the
    /// target's default container, the expression should be
    /// constructed using `new()` and `push()`.
    pub fn build<Func>(func: Func) -> Self
    where
        Func: FnOnce(&mut Arena<Target::Container>) -> AbsolutePos<Target>,
    {
        Self::try_build(|arena| -> Result<_, std::convert::Infallible> { Ok(func(arena)) }).unwrap()
    }

    pub fn try_build<Func, Error>(func: Func) -> Result<Self, Error>
    where
        Func: FnOnce(&mut Arena<Target::Container>) -> Result<AbsolutePos<Target>, Error>,
    {
        let mut arena = Arena::new();
        let root_ref = func(&mut arena)?;
        Ok(arena.finalize(root_ref))
    }
}
