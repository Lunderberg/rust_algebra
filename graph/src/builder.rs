use std::marker::PhantomData;

use crate::{
    BuilderToStorage, ContainedBy, NilRefType, RecursiveFamily, RecursiveObj, RecursiveRefType,
    Storage, TypedTree,
};

/// A constructor used to generate a `TypedTree<Container>`.
pub struct Builder<Container> {
    pub(crate) nodes: Vec<Container>,
}

impl<Container> Builder<Container> {
    /// Constructs an empty `Builder`.
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
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

impl<'a, Container: 'a> RecursiveRefType<'a> for Builder<Container> {
    type Ref<T: ?Sized> = BuilderRef<T>;
    type Value<T: 'a> = T;
}

impl<Container> Builder<Container> {
    /// Insert a new node to the builder
    pub fn push<
        'a,
        F: RecursiveFamily,
        T: RecursiveObj<'a, RefType = Builder<Container>, Family = F>,
    >(
        &mut self,
        builder_obj: T,
    ) -> BuilderRef<F::Obj<'a, NilRefType>>
    where
        F::Obj<'a, Storage>: ContainedBy<'a, Container>,
        F: RecursiveFamily<Obj<'a, Builder<Container>> = T>,
        Container: 'a,
    {
        let abs_pos = self.nodes.len();
        let converter = BuilderToStorage { size: abs_pos };
        let storage_obj: F::Obj<'a, Storage> = F::move_ref(builder_obj, &converter);
        let container: Container = storage_obj.to_container();
        self.nodes.push(container);
        BuilderRef {
            abs_pos,
            _node: PhantomData,
        }
    }
}

impl<'a, RootNodeType: RecursiveObj<'a>, Container> From<Builder<Container>>
    for TypedTree<'a, RootNodeType, Container>
{
    fn from(builder: Builder<Container>) -> Self {
        Self {
            nodes: builder.nodes,
            _phantom: PhantomData,
            _a: PhantomData,
        }
    }
}
