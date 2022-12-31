use std::marker::PhantomData;

use crate::{
    BuilderToStorage, ContainedBy, RecursiveFamily, RecursiveObj, RecursiveRefType, Storage,
    TypedTree,
};

pub struct Builder;

/// A constructor used to generate a `TypedTree<Container>`.
pub struct BuilderObj<Container> {
    pub(crate) nodes: Vec<Container>,
}

impl Builder {
    /// Constructs an empty `Builder`.
    pub fn new<Container>() -> BuilderObj<Container> {
        BuilderObj { nodes: Vec::new() }
    }
}

/// Reference type used while building a tree.  Any time the user
/// pushes a node into the builder, they receive a reference.
/// That reference may then be used to construct additional
/// builder nodes.
pub struct BuilderRef<T: ?Sized> {
    pub(crate) abs_pos: usize,
    _node: PhantomData<*const T>,
}

impl<'a> RecursiveRefType<'a> for Builder {
    type Ref<T: ?Sized> = BuilderRef<T>;
    type Value<T: 'a> = T;
}

impl<Container> BuilderObj<Container> {
    /// Insert a new node to the builder
    pub fn push<'a, F: RecursiveFamily, T: RecursiveObj<'a, RefType = Builder, Family = F>>(
        &mut self,
        builder_obj: T,
    ) -> BuilderRef<F::Obj<'a, Storage>>
    where
        F::Obj<'a, Storage>: ContainedBy<'a, Container>,
        F: RecursiveFamily<Obj<'a, Builder> = T>,
        Container: 'a,
    {
        let abs_pos = self.nodes.len();
        let converter = BuilderToStorage { size: abs_pos };
        let storage_obj: F::Obj<'a, Storage> = F::builder_to_storage(builder_obj, converter);
        let container: Container = storage_obj.to_container();
        self.nodes.push(container);
        BuilderRef {
            abs_pos,
            _node: PhantomData,
        }
    }
}

impl<'a, RootNodeType: RecursiveObj<'a, RefType = Storage>, Container> From<BuilderObj<Container>>
    for TypedTree<'a, RootNodeType, Container>
{
    fn from(builder: BuilderObj<Container>) -> Self {
        Self {
            nodes: builder.nodes,
            _phantom: PhantomData,
            _a: PhantomData,
        }
    }
}
