#![license = "MIT"]
#![experimental]

/*!
 * TODO:
 * ======
 * - (N, 1) Delete HazardList<T> after it's done being used.
 *   See hamt.rs for the solution to this.
 * - (D, 3) Write a version of local_delete in Hp::delete_scan()
 *   that allows for multiple HazardList<T>'s to exist at
 *   once.
 *      - This can be solved by making it a hashmap of
 *      (key, val) pairs; where the key is HazardList's
 *      address and the val is a list of addresses to be
 *      freed. This is a crappy method though.
 * - (N, 3) Cleanup task local delete list when a task exits.
 *      - This most likely involves a cleanup function being executed when
 *        the task quits. A function that moves the task-local list to a
 *        global list that allows other tasks to inherit the garabage of 
 *        the dead task.
 * - (N, 1) Use memory contention managed CAS in impl Hp<T> as the
 *   global CAS (HazardList does a lot of ops that could use it).
 * - (N, 1) In delete_scan remove entry from hashmap when it's
 *   vec.len() is 0.
 * - (N, 1) Reformat the functioning of the swap function.
 *
 * Credit
 * =======
 * Andrei Alexandrescu and Maged Michael (2004).
 *  "Lock-Free Data Structures with Hazard Pointers". Dr Dobbs. 
 *
 * Dave Dice, Danny Hendler, Ilya Mirsky (2013).
 *  "Lightweight Contention Management for Efficient Compare-and-Swap Operations".
 *  arXiv:1305.5800.
 */

use std::intrinsics::{atomic_xadd_relaxed, atomic_xsub_relaxed, atomic_cxchg_relaxed};
use std::collections::hashmap::{HashMap, HashSet};
use std::sync::atomics::{AtomicPtr, Relaxed};
use std::ptr::{mut_null, null};
use std::cell::RefCell;
use std::ty::Unsafe;
use std::io::Timer;
use std::cmp;
use std::mem;

/** 
 * Guarantees that what it points to will not be deleted or modified.
 */
pub struct ProtectedPointer<T> {
    _is_loaded: bool,
    _inner_ptr: *mut HpInner<T>, 
    _inner_val: *mut InnerProtect<T>
}

#[unsafe_destructor]
impl<T> Drop for ProtectedPointer<T> {
    fn drop(&mut self) {
        unsafe { (*self._inner_val).release(); }
    }
}

impl<T> Deref<T> for ProtectedPointer<T> {
    /// Dereferences any loaded pointer, fails!() otherwise.
    fn deref(&self) -> &T {
        if self._is_loaded { 
            unsafe { &*((*self._inner_val).ptr) }
        } else {
            fail!("A ProtectedPointer must be loaded before deref'ing");
        }
    }
}

impl<T> ProtectedPointer<T> {
    #[inline(always)]
    pub fn new(hp_ref: &Hp<T>) -> ProtectedPointer<T> {
        unsafe {
            ProtectedPointer {
                _is_loaded: false,
                _inner_ptr: hp_ref._inner_ptr, 
                _inner_val: (*(*hp_ref._inner_ptr)._head)
                            .get_protector(null())
            }
        }
    }

    /// Swap currently loaded ProtectedPointer out for an unloaded one.
    /// Does nothing otherwise.
    #[inline(always)]
    pub fn swap(&mut self, other: &mut ProtectedPointer<T>) {
        if !other._is_loaded && self._is_loaded { mem::swap(self, other); }
    }

    /// Loads the current value of the location the pointer is pointing to.
    /// For the load of any Hp the ProtectedPointer is pointing to succeed
    /// one of two conditions must be met; otherwise behavior is undefined.
    ///
    /// - The Hp must be owned and have a lifetime of at least as long as the
    ///   duration of the load.
    /// - The Hp must be protected by another ProtectedPointer for the duration
    ///   of the load.
    #[inline(always)]
    pub fn load(&mut self) {
        self._is_loaded = true;

        unsafe { 
            loop {
                (*self._inner_val).ptr = (*self._inner_ptr).get_data_ptr();

                if (*self._inner_val).ptr.to_uint() == 
                    (*self._inner_ptr).get_data_ptr().to_uint() {
                    break;
                }
            }
        }
    }

    #[inline(always)]
    pub fn unload(&mut self) {
        self._is_loaded = false;
    }

    /// Set the ProtectedPointer to point to a new Hp. A value for the new
    /// Hp isn't available until the ProtectedPointer is loaded once more.
    #[inline(always)]
    pub fn set(&mut self, new: &Hp<T>) {
        self._is_loaded = false;
        self._inner_ptr = new._inner_ptr;
    }
   
    /// Replace the value the ProtectedPointer is pointing to with another.
    /// Succeeds iff the value the ProtectedPointer is pointing to is the
    /// current value of the Hp.
    #[inline(always)]
    pub fn replace(&mut self, new: T) -> Result<(), ()> {
        if self._is_loaded {
            unsafe { (*self._inner_ptr).replace(self._inner_val, new) }
        } else {
            fail!("A ProtectedPointer must be loaded before replacing");
        }
    }
}

/**
 * A pointer that needs to be protected in order to read/write
 */
pub struct Hp<T> {
    _inner_ptr: *mut HpInner<T>
}

impl<T> Clone for Hp<T> {
    fn clone(&self) -> Hp<T> {
        unsafe { atomic_xadd_relaxed(&mut (*self._inner_ptr)._inbound, 1); }
        Hp { _inner_ptr: self._inner_ptr }
    }
}

#[unsafe_destructor]
impl<T> Drop for Hp<T> {
    fn drop(&mut self) {
        unsafe {
            // If this was the last Hp pointing to HpInner delete everything.
            if atomic_xsub_relaxed(&mut (*self._inner_ptr)._inbound, 1)  == 0 {
                
                // Safely delete internal data.
                let data = self.get_data_ptr();
                self.delete_scan(data); 
            }
        }
    }
}

impl<T> Hp<T> {
    /// Initializes a Hp<T> that should be used as the root of a data strcuture
    /// whose deletions are safely managed by the HazardPointer library
    /// 
    /// Effects
    /// ===========
    /// Creates an internal linkedlist to track what data is being used at the momment.
    /// The list is deleted upon deletion of the Hp created with init()
    #[inline(always)]
    pub fn init(data: T) -> Hp<T> {
        let new_head: Box<HazardList<T>> = box HazardList::new();

        let new_inner: Box<HpInner<T>> = box HpInner {
            _head: unsafe { mem::transmute::<_, *mut HazardList<T>>(new_head) },
            _inbound: 1,
            _outbound: 1,
            _data_ptr: Unsafe::new(unsafe { mem::transmute::<_, *const T>(box data) })
        };

        Hp { _inner_ptr: unsafe { mem::transmute::<_, *mut HpInner<T>>(new_inner) } }
    }

    /// Used to interconnect data structures connected with a preexisting
    /// Hp<T>.
    ///
    /// Effects
    /// ===========
    /// Returns a Hp<T> whose internal data is managed by the same
    /// internal hazardhead.
    #[inline(always)]
    pub fn new(&self, data: T) -> Hp<T> {
        let new_inner: Box<HpInner<T>> = box HpInner {
            _head: unsafe { (*self._inner_ptr)._head },
            _inbound: 1,
            _outbound: 1,
            _data_ptr: Unsafe::new(unsafe { mem::transmute::<_, *const T>(box data) })
        };

        Hp { _inner_ptr: unsafe { mem::transmute::<_, *mut HpInner<T>>(new_inner) } }
    }

    #[inline(always)]
    unsafe fn delete_scan(&self, add: *const T) {
        (*self._inner_ptr).delete_scan(add);
    }
    
    #[inline(always)]
    fn get_data_ptr(&self) -> *const T { 
        unsafe { *((*self._inner_ptr)._data_ptr.get()) as *const T }
    }
}

// Inbound == 0: HpInner is marked for deletion.
// Outbound == 0: HpInner is deleted.
struct HpInner<T> {
    _head: *mut HazardList<T>,
    _inbound: uint,
    _outbound: uint,
    _data_ptr: Unsafe<*const T>
}

impl<T> HpInner<T> {
    /// Attempt to replace head of old with new. If true, free
    /// old; else, free new. Frees are held at task-local 
    /// granularity until they don't present a hazard.
    ///
    /// Old or new data is added to a task-local vec until it can be 
    /// safely deleted. If the replacement fails an HpErr is returned
    /// describing the reasons why.
    #[inline(always)]
    fn replace(&mut self, current: *mut InnerProtect<T>, new: T) -> Result<(), ()> {
        unsafe {
            debug_assert!(!(*current).ptr.is_null(), "ProtectedPointer never set!");

            let new_ref = mem::transmute::<_, *const T>(box new);
            let old_ref = (*current).ptr;
            
            if self.compare_and_swap(old_ref, new_ref) {
                // There is now one more potential back-reference to HpInner
                atomic_xadd_relaxed(&mut self._outbound, 1); 
                self.delete_scan(old_ref);
                Ok(())
            } else {
                drop(mem::transmute::<_, Box<T>>(new_ref));
                Err(())
            }
        }
    }

    /// Unsafe internal compare and swap with basic memory contention
    /// management at granularity of thread-local variables.
    #[inline(never)]
    unsafe fn compare_and_swap(&self, old: *const T, new: *const T) -> bool {
        #[thread_local] 
        static mut failures: uint = 0;
        static c: uint = 9;
        static m: uint = 27;
        static exp_threshold: uint = 2;

        if atomic_cxchg_relaxed(self._data_ptr.get() as *mut uint, old as uint, new as uint) == old as uint {
            if failures > 0 { atomic_xsub_relaxed(&mut failures, 1); }
            true
        } else {
            atomic_xadd_relaxed(&mut failures, 1);
            // Block task exponentially.
            if failures > exp_threshold {
                let mut timer = Timer::new().unwrap();
                timer.sleep(1 << ( cmp::min((c*failures), m) / 1000000 ));
            }
            false
        }
    }

    /// Unsafe internal delete function that ensures no hazards exist
    /// for items on the list before calling their 'drop' method to
    /// delete the item permanently.
    #[inline(never)]
    unsafe fn delete_scan(&self, add: *const T) {
        local_data_key!(local_delete: RefCell<HashMap<uint, Vec<(uint, uint)>>>);
        
        let head_size = self.get_hazard_list().length;
        
        // Add new data to task-local delete list.
        let local_size = if local_delete.get().is_none() {
            let mut temp_map = HashMap::with_capacity(1);
            let mut temp_vec = Vec::with_capacity(2);

            temp_vec.push(((self as *const HpInner<T>) as uint, add as uint));
            temp_map.insert(self._head as uint, temp_vec);

            local_delete.replace(Some(RefCell::new(temp_map)));
            1
        } else {
            let dls = local_delete.get().unwrap();
            match dls.try_borrow_mut() {
                Some(mut map) =>
                    map.find_with_or_insert_with( 
                        self._head as uint,
                        ((self as *const HpInner<T>) as uint, add as uint),
                        |_, vec, new| vec.push(new),
                        |_, new| vec!(new))
                       .len(),
                None => 0
            }
        };

        // See if we should attempt deletion this call.
        if local_size >= (head_size / 4) {
            let mut hlist = self.get_hazard_list().hazards.load(Relaxed);
            let mut hashset: HashSet<uint> = HashSet::with_capacity(local_size + head_size);

            // Insert all hazards into a hashset.
            while !hlist.is_null() {
                let curr_ptr = (*hlist).ptr;
                if !curr_ptr.is_null() { 
                    hashset.insert(curr_ptr as uint);
                }
                hlist = (*hlist).next;
            } 

            // Delete any data on local_delete that doesn't intersect with the hashset
            // The borrow of the RefCell will fail when the calling thread exits.
            match local_delete.get().unwrap().try_borrow_mut() {
                Some(mut map) => 
                    match map.find_mut(&(self._head as uint)) {
                        Some(arr) => arr.retain(|&(ref inner, ref x)| {
                            if !hashset.contains(x) {
                                drop(mem::transmute::<_, Box<T>>(*x as *mut T));

                                // Iff the outbound weight is 0 can we safely delete an HpInner
                                // as there can't be any possible references to it at this point.
                                if atomic_xsub_relaxed(&mut (*(*inner as *mut HpInner<T>))._outbound, 1) == 0 {
                                    drop(mem::transmute::<_, Box<HpInner<T>>>(*inner as *mut HpInner<T>));
                                }
                                false
                            } else { true }}),
                        _ => ()
                    },
                _ => ()
            }
        }
    }

    #[inline(always)]
    fn get_data_ptr(&self) -> *const T { 
        unsafe { *(self._data_ptr.get()) as *const T }
    }

    #[inline(always)]
    fn get_hazard_list<'a>(&'a self) -> &'a mut HazardList<T> {
        unsafe { &mut *self._head }
    }
}

/**
 * # Purpose 
 * Used as a global list of currently protected pointers 
 * of a given type T in HazardList<T>.
 */
struct InnerProtect<T> {
    active: int,
    ptr: *const T,
    next: *mut InnerProtect<T>
}

impl<T> InnerProtect<T> {
    #[inline(always)]
    fn new() -> *mut InnerProtect<T> {
        let new_inner: Box<InnerProtect<T>> =
            box InnerProtect {  active: 1,
                                ptr: null(),
                                next: mut_null() };
        unsafe { mem::transmute::<_, *mut InnerProtect<T>>(new_inner) }
    }

    #[inline(always)]
    unsafe fn acquire(&mut self) -> bool {
        self.active == 0 && atomic_cxchg_relaxed(&mut self.active, 0, 1) == 0
    }

    #[inline(always)]
    fn release(&mut self) {
        self.ptr = null();
        self.active = 0;
    }
}

/**
 * HazardList generates and manages protected pointers to type
 * Hp<T> that ensure that any data pointed to by Hp<T> that is being
 * read has it's deletion delayed until all reads have concluded.
 */
struct HazardList<T> {
    length: uint,
    hazards: AtomicPtr<InnerProtect<T>>
}

impl<T> HazardList<T> {
    #[inline(always)]
    fn new() -> HazardList<T> {
        HazardList {
            length: 0,
            hazards: AtomicPtr::new(mut_null())
        }
    }

    /// Acquire protection against deletes of data at T.
    fn get_protector(&mut self, ele: *const T) -> *mut InnerProtect<T> {
        let mut hlist = self.hazards.load(Relaxed);

        // Try to acquire an existing protector
        while !hlist.is_null() {
            unsafe {
                if (*hlist).acquire() {
                    (*hlist).ptr = ele; 
                    return hlist; 
                }
                hlist = (*hlist).next; 
            }
        }

        // Otherwise create a new one
        let new_protector = InnerProtect::new();
        unsafe { atomic_xadd_relaxed(&mut (self.length), 1); }
        
        // Insert new protector
        loop {
            hlist = self.hazards.load(Relaxed);
            unsafe { (*new_protector).next = hlist; }

            if self.hazards.compare_and_swap(hlist, new_protector, Relaxed) == hlist { break; }
        }

        unsafe { (*new_protector).ptr = ele; }
        new_protector
    }
}


#[cfg(test)]
mod tests {
    extern crate native;

    use std::task::TaskBuilder;
    use self::native::NativeTaskBuilder;

    use super::{Hp, ProtectedPointer};

    #[test]
    fn vec_int() {
        let a_lis: Hp<Vec<int>> = Hp::init(Vec::new());
        let num_threads = 30i;
        
        // Insert and print num_threads elements concurrently.
        for i in range(0, num_threads) {
            let b_lis = a_lis.clone();

            TaskBuilder::new().native().spawn(proc() {
                // Generate a pointer that can safely read
                // a Hp<T> state.
                let mut value_ptr = ProtectedPointer::new(&b_lis);
               
                for x in range(0, 1000i) {
                loop {
                    // Read in the current state of Hp<T> and acquire a
                    // local verison of Hp<T> so we can modify the structure.
                    value_ptr.load();

                    // Create a new local state.
                    let mut new_vec = value_ptr.clone();
                    
                    // Modify the local state.
                    new_vec.push(x);
                    
                    // Attempt to update global state with the local one.
                    match value_ptr.replace(new_vec) {
                        Ok(_) => break,
                        _ => ()
                    };
                }}

            });
        }
        
        // Read in current vec list
        let mut value_ptr = ProtectedPointer::new(&a_lis);
        
        // Wait until all threads have made their writes
        loop {
            value_ptr.load();

            // There should exist num_threads elements in the vec
            if value_ptr.len() == num_threads as uint * 1000u  { break; }
        }
    }
}
