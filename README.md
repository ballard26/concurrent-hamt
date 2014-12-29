Lock-free hash array mapped trie in Rust
----------------------------------------------------------------------
[![Build Status](https://travis-ci.org/ballard26/concurrent-hamt.svg?branch=master)](https://travis-ci.org/ballard26/concurrent-hamt)

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
git = "https://github.com/ballard26/concurrent-hamt"
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

Todo
----------------------------------------------------------------------
Check the top of every source file for a todo list. All comments and 
contributions are more than welcome.

Notes
----------------------------------------------------------------------
Compatibility
- This project was only tested on OS X and Linux, no guarantee it will work on Windows.
- As with above it was also only tested on x64_86 and x86. It will most likely work on 
  ARM platforms supported by the LLVM atomic intrinsics and Rust's standard lib.

Error Handling
- Any errors that arise from deterministic misuse problems will fail!().
    E.g. a ProtectedPointer must be loaded before it's read, loads never 
    fails if usage conditions are met, hence the program will fail!() if
    an unloaded ProtectedPointer is read.
- All other errors are handled with the Result type since they arise
  due to the non-deterministic nature of a specific event.
    E.g. when replacing a value in ProtectedPointer it is impossible to determine
    if a value you expect to be there still is, hence a Result value is returned.

Sources Referenced
----------------------------------------------------------------------
Bagwell, Phil (2000). "Ideal Hash Trees".
<br>&nbsp;&nbsp;&nbsp;Infoscience Department, École Polytechnique Fédérale de Lausanne.
<br>Prokopec, A., Bronson N., Bagwell P., Odersky M. (2011).
<br>&nbsp;&nbsp;&nbsp;"Concurrent Tries with Efficient Non-Blocking Snapshots".
<br>Andrei Alexandrescu and Maged Michael (2004).
<br>&nbsp;&nbsp;&nbsp;"Lock-Free Data Structures with Hazard Pointers". Dr Dobbs.
<br>Dave Dice, Danny Hendler, Ilya Mirsky (2013).
<br>&nbsp;&nbsp;&nbsp;"Lightweight Contention Management for Efficient Compare-and-Swap Operations".
<br>&nbsp;&nbsp;&nbsp;arXiv:1305.5800.
