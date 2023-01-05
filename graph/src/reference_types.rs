use std::fmt::{Display, Formatter};
use std::marker::PhantomData;

use crate::{ContainerOf, RecursiveFamily, RecursiveObj, RecursiveRefType};

/// A usage annotation for objects that are being constructed.
pub struct Builder;

impl RecursiveRefType for Builder {
    type Ref<'a, T: 'a> = BuilderRef<T>;
    type Value<'a, T: 'a> = T;
}

/// Reference type used while building a tree.  Any time the user
/// pushes a node into the builder, they receive a reference.
/// That reference may then be used to construct additional
/// builder nodes.
pub struct BuilderRef<T> {
    pub(crate) abs_pos: usize,
    pub(crate) _node: PhantomData<*const T>,
}

/// A usage annotation for objects that may be stored in the
/// linearized structure.
pub struct Storage;

impl RecursiveRefType for Storage {
    type Ref<'a, T: 'a> = StorageRef<T>;
    type Value<'a, T: 'a> = T;
}

/// A reference in the linearized structure.
pub struct StorageRef<T> {
    /// The location of the referred-to node, relative to the node
    /// holding the reference, in the direction of the start of the
    /// `TypedTree`.
    ///
    /// `rel_pos` is unsigned, in order to avoid needing
    /// loop-detection when walking through a graph.  In the
    /// linearized structure, the index of a referent is strictly less
    /// than the index of the node holding the reference, making
    /// reference loops unrepresentable.
    pub(crate) rel_pos: usize,
    pub(crate) _node: PhantomData<*const T>,
}

/// A usage annotation for objects that contain a view into the full
/// tree structure that they represent.
pub struct Visiting<'a, Container: 'a> {
    _a: PhantomData<&'a usize>,
    _c: PhantomData<*const Container>,
}

/// Relative backreference to earlier node.  Used to represent
/// references into recursively-defined structures while traversing
/// the graph.
pub struct VisitingRef<'a, T, Container: 'a> {
    /// The reference to be followed, relative to the last element in
    /// `view`.
    pub(crate) rel_pos: usize,

    /// The subgraph in which the reference points.  The reference is
    /// relative to the last item in the view.
    pub(crate) view: &'a [Container],
    pub(crate) _phantom: PhantomData<*const T>,
}

impl<'b, Container> RecursiveRefType for Visiting<'b, Container> {
    type Ref<'a, T: 'a> = VisitingRef<'b, T, Container>;
    type Value<'a, T: 'a> = &'a T;
}

impl<T> BuilderRef<T> {
    /// Convert the builder reference (absolute positioning) to a
    /// storage reference (relative positioning).
    pub fn to_storage(&self, new_pos: usize) -> StorageRef<T> {
        let rel_pos = new_pos
            .checked_sub(self.abs_pos)
            .expect("Invalid reference type");
        StorageRef {
            rel_pos,
            _node: PhantomData,
        }
    }
}

impl<T> StorageRef<T> {
    /// Convert the storage reference (relative positioning without
    /// view) to a visiting reference (relative positioning with
    /// view).
    pub fn to_visiting<'a, Container>(
        &'a self,
        view: &'a [Container],
    ) -> VisitingRef<'a, T, Container> {
        VisitingRef {
            rel_pos: self.rel_pos,
            view,
            _phantom: PhantomData,
        }
    }
}

/// Implementation of Clone for any VisitingRef.  The #[derive(Clone)]
/// macro can't be used here, because it requires that all generic
/// parameters implement Clone.  Therefore, even though the T and
/// Container parameters only appear as a copy-able reference and a
/// copy-able PhantomData, the automatic implementation wouldn't be
/// generated.
impl<'a, T, Container> Clone for VisitingRef<'a, T, Container> {
    fn clone(&self) -> Self {
        VisitingRef {
            rel_pos: self.rel_pos,
            view: self.view,
            _phantom: self._phantom,
        }
    }
}

impl<'a, T, Container> Copy for VisitingRef<'a, T, Container> {}

impl<'a, NodeType, Container> Display for VisitingRef<'a, NodeType, Container>
where
    NodeType: RecursiveObj<'a, RefType = Storage>,
    Container: ContainerOf<NodeType>,
    <NodeType::Family as RecursiveFamily>::Obj<'a, Visiting<'a, Container>>: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.borrow().unwrap())
    }
}
