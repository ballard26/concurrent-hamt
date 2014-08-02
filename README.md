 [![Build Status](https://travis-ci.org/ballard26/concurrent-hamt.svg?branch=master)](https://travis-ci.org/ballard26/concurrent-hamt)
Concurrent, wait-free, non-blocking, hash array map trie in Rust
----------------------------------------------------------------------

A naive/experimental implementation of a concurrent HAMT using 
HazardPointers for safe memory deallocation and an exponential back-off 
algorithm for basic memory contention management.

Usage
----------------------------------------------------------------------
To use create a new HAMT then simply clone it into any process. The clones
connect to the same structure and once all clones and the original are
dropped the structure will be deallocated.

Add this to your Cargo.toml:
```toml
[dependencies.hamt]
git = "https://github.com/ballard26/rust-concurrent-hamt"
```

And then an example usage:
```rust
extern crate hamt;

use hamt::hamt::HAMT;

fn main() {
    let amt: HAMT<uint, uint> = HAMT::new();

    for a in range(0, 10u) {
        let cloned_amt = amt.clone();
        spawn(proc() {
            for x in range(a*100, (a+1)*100u) { 
                cloned_amt.insert(a, a);
            }
        });
    }
}
```

Performance
----------------------------------------------------------------------
Horrible.

Todo
----------------------------------------------------------------------
Check the top of every source file for a todo list. 

Notes
----------------------------------------------------------------------
Credit
- See top of source files for citations to research papers I referenced while
  writing code in file.

Compatibility
- This project was developed and tested on OS X, however, since it doesn't rely
  on any OS dependent libs it is very likely to work on Linux and maybe Windows.
- As with above it was also developed on x64_86, it will work on the 32bit equiv.,
  and will most likely work on ARM platforms supported by the LLVM atomic 
  intrinsics and Rust's standard lib.

Error Handling
- Any errors that arise from deterministic misuse problems will fail!().
    E.g. a ProtectedPointer must be loaded before it's read, loads never 
    fails if usage conditions are met, hence the program will fail!() if
    an unloaded ProtectedPointer is read.
- All other errors are handled with the Result type since they arise
  due to the non-deterministic nature of a specific event.
    E.g. when replacing a value in ProtectedPointer it is impossible to determine
    if a value you expect to be there still is, hence a Result value is returned.
