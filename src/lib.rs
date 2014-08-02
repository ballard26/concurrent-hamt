#![feature(thread_local, unsafe_destructor)]
#![crate_name = "hamt"]
#![crate_type = "lib"]
#![license = "MIT"]
#![experimental]

/* Lets see how many buzzwords we can fit... */
//! A Concurrent, persistent, wait-free, non-blocking, hash map array trie.

pub mod hp;
pub mod hamt;
