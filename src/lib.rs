#![feature(thread_local, unsafe_destructor)]
#![crate_name = "hamt"]
#![crate_type = "lib"]
#![license = "MIT"]
#![experimental]

//! A Concurrent, persistent, wait-free, non-blocking, hash map array trie.

pub mod hamt;
pub mod hp;
mod bits;
