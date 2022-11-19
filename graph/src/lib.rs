//! Recursive data structures in contiguous memory.
//!
//! A collection of traits to represent recursive data structures
//! within a single block of contiguous memory.  These these data
//! structures can be represented using `Box`, doing so requires
//! additional memory allocations to construct, and additional pointer
//! dereferences to traverse.
//!
//! These traits are intended to be generated with macros provided in
//! `graph_derive`,

extern crate self as graph;

mod graph_traits;
pub use graph_traits::*;

mod error;
pub use error::*;
