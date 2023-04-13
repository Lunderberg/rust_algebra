//! Recursive data structures in contiguous memory.
//!
//! A utility to represent recursive data structures within a single
//! block of contiguous memory.  While these data structures can be
//! represented using `Box`, doing so requires additional memory
//! allocations to construct, and additional pointer dereferences to
//! traverse.
//!
//! The linearized structures and traits can be generated with macros
//! provided in `graph_derive`,

extern crate self as graph;

mod error;
pub use error::*;

mod utility_traits;
pub use utility_traits::*;

mod recursive_types;
pub use recursive_types::*;

mod reference_types;
pub use reference_types::*;

mod reference_converters;
use reference_converters::*;

mod visitor_trait;
pub use visitor_trait::*;

mod arena;
pub use arena::*;
