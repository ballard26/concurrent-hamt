#![license = "MIT"]
#![experimental]

/*!
 * TODO 
 * ======
 * - (D, 1) Ensure drops chain, e.g. Vec< Hp<T> > will call Hp's drop
 *   when it is dropped.
 * - (N, ?) Return a ProtectedPointer to data instead of a clone. This will
 *   allow use to drop the 'Clone' trait requirement from K and V.
 *      - This has a few problems attached with it mainly that the
 *        size of the ProtectedPointer list in HazardHead becomes
 *        dependent on the end-user.
 * - (N, 1) Make HazardHead in hp.rs pub and add it to HAMT, this with allow
 *   us to delete it when the HAMT is deleted(right now it isn't being
 *   deleted at all).
 * - (N, 2) Implement a 'count' function to Bitv that uses POPCNT instruction.
 * - (N, ?) Use index resizing to bring search from O(log(n)) -> O(1).
 *      - Resizing makes sense on a mutable imperative structure, however,
 *        the naive way of doing it is detrimental to a persistent functional
 *        one in which copying is commonplace. 
 * - (N, 1) Implement traits from std::Collections.
 * - (N, 3) Have a LinkedNode for when level exceeds capacity of a uint.
 * - (N, ?) Figure out the stack-overflow problems that mat result from dropping the 
 *   head of a large tree(lots of function calls from drop glue deallocating
 *   space).
 * - (N, 2) Refactor the trie contracting in repair_deleted_node(..).
 *
 * Credit
 * =======
 * Bagwell, Phil (2000). "Ideal Hash Trees". 
 *  Infoscience Department, École Polytechnique Fédérale de Lausanne.
 *
 * Prokopec, A., Bronson N., Bagwell P., Odersky M. (2011).
 *  "Concurrent Tries with Efficient Non-Blocking Snapshots".
 */

use std::hash::{Hash, Hasher, sip};
use std::collections::bitv::Bitv;
use hp::{ProtectedPointer, Hp};
use std::rand;

// Settings for 64bit (0x3f, 6, 64, 64). 
static index_map: uint = 0x1f;
static index_size: uint = 5;
static hash_size: uint = 64;
static map_size: uint = 32;

pub struct HAMT<K,V> {
    hash_base: u64,
    root: Hp<HAMTInner<K,V>>
}

// Any 'Deleted' state can't be modified directly, and must be
// changed through it's parent node(the root node is never set
// to a deleted state).
//
// Below are possible states of an inner node for HAMT, above
// each state are the possible states it can transition into.
enum HAMTInner<K, V> {
    // RootedNode -> RootedNode: Add/remove an element to vec.
    RootedNode(Bitv, Vec<Hp<HAMTInner<K,V>>>),
    // MappedNode -> MappedNode: Add/remove an element to vec.
    // MappedNode -> DeletedMap: vec.len() < 2.
    MappedNode(Bitv, Vec<Hp<HAMTInner<K,V>>>),
    // ValuedNode -> MappedNode: collision with another ValuedNode
    // ValuedNode -> DeletedVal: removal from HAMT.
    ValuedNode(K, V),
    // Parent is modified to include any vals in vec.
    DeletedMap(Bitv, Vec<Hp<HAMTInner<K,V>>>),
    // Parent is modified to remove DeletedVal from vec.
    DeletedVal
    // A delayed resolution of conflicting indexes in hash until
    // insert of values in vec don't require a depth increase.
    //PromiseVal(uint, uint, Hp<HAMTInner<K,V>>),
}

impl<K,V> Clone for HAMT<K,V> {
    fn clone(&self) -> HAMT<K,V> {
        HAMT {
            hash_base: self.hash_base,
            root: self.root.clone()
        }
    }
}

impl<K: Hash + PartialEq + Clone, V: Clone> HAMT<K,V> {
    pub fn new() -> HAMT<K,V> {
        HAMT {
            hash_base: rand::random::<u64>(),
            root: Hp::init(RootedNode(
                    Bitv::with_capacity(map_size, false),
                    Vec::with_capacity(1)))
        }
    }

    /// Guaranteed to find any value existent in the hamt before and during
    /// the function call.
    pub fn search(&self, key: K) -> Option<V> {
        let mut pre_node = ProtectedPointer::new(&self.root);
        let mut cur_node = ProtectedPointer::new(&self.root);
        let mut state = SearchState::new(self, &key);

        'search: loop {
            cur_node.swap(&mut pre_node);
            cur_node.load();

            match *cur_node {
                MappedNode(ref map, ref arr) 
                | DeletedMap(ref map,ref arr) 
                | RootedNode(ref map, ref arr) => 
                    match state.compressed_index(map) {
                        Ok(idx) => {
                            pre_node.set(&arr[idx]);

                            state.next(); 
                            continue 'search; 
                        } 
                        Err(_) => return None 
                    }, 
                ValuedNode(ref k, ref v) if *k == key => 
                    return Some(v.clone()), 
                _ => return None
            }; 
        }
    }

    /// Guaranteed to insert key/value into hamt.
    pub fn insert(&self, key: K, value: V) {
        'start: loop {
            let mut cur_node = ProtectedPointer::new(&self.root);
            let mut pre_node = ProtectedPointer::new(&self.root);

            let ins_node = self.root.new(ValuedNode(key.clone(), value.clone()));
            let mut state = SearchState::new(self, &key);

            'search: loop {
                cur_node.swap(&mut pre_node);
                cur_node.load();

                // Map current node to new node.
                let (new_node, cont_search) = match *cur_node {
                    ref node @ MappedNode(ref map, ref arr) 
                    | ref node @ RootedNode(ref map, ref arr) =>
                        match state.compressed_index(map) {
                            Ok(idx) => {
                                pre_node.set(&arr[idx]);

                                state.next();
                                continue 'search;
                            }
                            Err(idx) => {
                                let mut bitmap = map.clone();
                                bitmap.set(state.uncompressed_index(&key), true);

                                let mut arrmap = arr.clone();
                                arrmap.insert(idx, ins_node.clone());

                                match *node {
                                    MappedNode(..) =>
                                        (MappedNode(bitmap, arrmap), false),
                                    _ =>
                                        (RootedNode(bitmap, arrmap), false)
                                }
                            }
                        },
                    ValuedNode(ref k, ref v) =>
                        if *k == key {
                            return;
                        } else {
                            let mut bitmap = Bitv::with_capacity(map_size, false);
                            bitmap.set(state.uncompressed_index(k), true);

                            let mut arrmap = Vec::with_capacity(1);
                            arrmap.insert(0, self.root.new(ValuedNode(k.clone(), v.clone())));
                            
                            (MappedNode(bitmap, arrmap), true)
                        },
                    DeletedMap(ref map, ref arr) => {
                        self.repair_deleted_node(&mut pre_node, state.prev(),
                        if arr.len() > 0 {
                                Some((map, arr))
                            } else {
                                 None
                            });
                        continue 'start;
                    }
                    DeletedVal => {
                        self.repair_deleted_node(&mut pre_node, state.prev(), None);
                        continue 'start;
                    }
                };

                match cur_node.replace(new_node) {
                    Ok(_) =>
                        if cont_search { continue 'search }
                        else { return },
                    // Shouldn't  I be able to continue search here?
                    Err(_) => continue 'start 
                };
            }
        }
    }

    // At the moment the structure is being lazily repaired.
    // E.g. remove is simply replacing the Deleted(Map/Val) with 
    // nothing then letting the next remove/insert repair the
    // parent. This has a chance of wasting space, if it does
    // one can just have remove attempt to repair right away.

    /// Guaranteed to remove any value existent in the hamt before and during
    /// the function call.
    pub fn remove(&self, key: K) -> bool {
        'start: loop {
            let mut cur_node = ProtectedPointer::new(&self.root);
            let mut pre_node = ProtectedPointer::new(&self.root);

            let mut state = SearchState::new(self, &key);

            'search: loop {
                cur_node.swap(&mut pre_node);
                cur_node.load();

                // Map current node to new node.
                let new_node = match *cur_node {
                    MappedNode(ref map, ref arr) 
                    | RootedNode(ref map, ref arr) =>
                        match state.compressed_index(map) {
                            Ok(idx) => {
                                pre_node.set(&arr[idx]);

                                state.next();
                                continue 'search;
                            }
                            Err(_) => return false
                        },
                    ValuedNode(ref k, _) =>
                        if *k == key {
                            DeletedVal
                        } else {
                            return false;
                        }, 
                     DeletedMap(ref map, ref arr) => {
                        self.repair_deleted_node(&mut pre_node, state.prev(),
                           if arr.len() > 0 {
                                Some((map, arr))
                            } else {
                                 None
                            });
                        continue 'start;
                    }
                    DeletedVal => {
                        self.repair_deleted_node(&mut pre_node, state.prev(), None);
                        continue 'start;
                    }
                };
                
                // Attempt to replace the current known value of writer current
                // with the newly created value.
                match cur_node.replace(new_node) {
                    Ok(_) => {
                        self.repair_deleted_node(&mut pre_node, state.prev(), None);
                        return true;
                    }
                    // Shouldn't  I be able to continue search here?
                    Err(_) => continue 'start
                };
            }
        }
    }

    // Remove a value node from the parent map.
    #[inline(always)]
    fn repair_deleted_node<'a>(&self,
                               value: &mut ProtectedPointer<HAMTInner<K,V>>,
                               sstate: &mut SearchState<'a, K>,
                               insert: Option<(&Bitv, &Vec<Hp<HAMTInner<K,V>>>)>
                               ) -> bool {
        let (new_node, ret_val) = match **value {
            ref node @ MappedNode(ref map, ref arr) 
            | ref node @ RootedNode(ref map, ref arr) =>
                match sstate.compressed_index(map) {
                    Ok(idx) => {
                        let mut arrmap = arr.clone();
                        let mut bitmap = map.clone();
                        arrmap.remove(idx);

                        match insert {
                            Some((map, arr)) => {
                                let val = &arr[0];
                                let mut temp_value = ProtectedPointer::new(val);
                                // Load is fine since calling function is protecting the Hp
                                temp_value.load();

                                // See if remaining element in a DeletedMap could be contracted.
                                match *temp_value {
                                    MappedNode(..)
                                    | DeletedMap(..) =>
                                        arrmap.insert(idx, val.new(MappedNode(map.clone(), arr.clone()))),
                                    // It is safe to push up a reference to a ValuedNode since
                                    // nothing will modify it until the DeletedMap is fixed.
                                    ValuedNode(..) =>
                                        arrmap.insert(idx, val.clone()),
                                    DeletedVal =>
                                        bitmap.set(sstate.uncompressed_index(sstate.curr_key), false),
                                    _ => fail!("Unexpected value!")
                                }
                            }
                            None =>
                                bitmap.set(sstate.uncompressed_index(sstate.curr_key), false)
                        };

                        // MappedNode's less than a certain length should be
                        // checked for potential contractions. While RootedNode's
                        // should always be mapped to themselves.
                        match *node {
                            MappedNode(..) =>
                                if arrmap.len() < 2 {
                                    (DeletedMap(bitmap, arrmap), false)
                                } else {
                                    (MappedNode(bitmap, arrmap), true)
                                },
                            RootedNode(..) => (RootedNode(bitmap, arrmap), true),
                            _ => fail!("Unexpected value!")
                        }
                    }
                    _ => return false
                },
            _ => return false
        };

        // Note there is no guarantee that the cur_node
        // is pointing to valid data or not. However, that
        // doesn't matter, either the current address is the 
        // same as the previously read one, or it changed.
        // We don't bother reading the changed one, hence it
        // doesn't matter if it's valid.
        match value.replace(new_node) {
            Ok(_) => ret_val,
            Err(_) => false
        }
    }
}

/**
 * Internal state that defines all vars
 * needed to traverse the HAMT.
 */
struct SearchState<'a, K> {
    hash_gen: sip::SipHasher,
    hash_base: u64,
    curr_key: &'a K,
    curr_lvl: uint,
    curr_hsh: u64,
    bits_lft: uint
}

impl<'a, K: Hash> SearchState<'a, K> {
    #[inline(always)]
    fn new<V>(root: &HAMT<K,V>, key: &'a K) -> SearchState<'a, K> {
        let mut new_state = SearchState {
            hash_gen: sip::SipHasher::new_with_keys(root.hash_base, 0),
            hash_base: root.hash_base,
            curr_key: key,
            curr_lvl: 0,
            curr_hsh: 0,
            bits_lft: hash_size 
        };

        new_state.curr_hsh = new_state.hash_gen.hash(key);
        new_state
    }

    #[inline(always)]
    fn next(&mut self) {
        self.bits_lft -= index_size;
        self.curr_lvl += 1;

        // Rehash if needed.
        if self.bits_lft < index_size {
            self.hash_gen =  sip::SipHasher::new_with_keys(self.hash_base, self.curr_lvl as u64);
            self.curr_hsh = (self.hash_gen).hash(self.curr_key);
            self.bits_lft = hash_size;
        }
    }

    /// This is probably wrong! Need to correct before implementing the
    /// DelayedRes type in HAMTInner<K,V>.
    #[inline(always)]
    fn prev<'b>(&'b mut self) -> &'b mut SearchState<'a, K> {
        // Undo a rehash if needed.
        if self.bits_lft == hash_size {
            let lvls_per_hash: uint = hash_size / index_size;
            
            self.hash_gen = sip::SipHasher::new_with_keys(self.hash_base, (self.curr_lvl - lvls_per_hash) as u64);
            self.curr_hsh = (self.hash_gen).hash(self.curr_key);
            self.bits_lft = index_size + (hash_size - lvls_per_hash); // Ensure this is right!
        } else {
            self.bits_lft += index_size;
        }

        self.curr_lvl -= 1;
        self
    }

    #[inline(always)]
    fn compressed_index(&mut self, bitmap: &Bitv) -> Result<uint, uint> {
        let uncompressed_index = (self.curr_hsh >> (self.curr_lvl * index_size)) 
                                    & index_map as u64;

        // Change to POPCNT.
        let compressed_index = bitmap.iter()
                                     .skip((uncompressed_index+1) as uint)
                                     .filter(|x| *x)
                                     .count();
        
        if bitmap.get(uncompressed_index as uint) {
            Ok(compressed_index) 
        } else { 
            Err(compressed_index) 
        }
    }

    #[inline(always)]
    fn uncompressed_index(&mut self, key: &K) -> uint {
        let hash = self.hash_gen.hash(key);
        ((hash >> (self.curr_lvl * index_size)) & index_map as u64) as uint
    }
}

#[cfg(test)]
mod tests {
    extern crate native;
    extern crate sync;

    use self::native::NativeTaskBuilder;
    use self::sync::{Arc, Barrier};
    use std::task::TaskBuilder;
    use super::HAMT;
    
    static num_threads: uint = 8; 
    static num_elements: uint = 10000;

    #[test]
    fn all_accounted_for() {
        let amt: HAMT<uint, uint> = HAMT::new();
        for i in range(0, num_elements) { amt.insert(i, i); };
        for i in range(0, num_elements) { 
            if amt.search(i).is_none() { 
                fail!("Well this is embarrassing."); 
            } 
        }
    }

    #[test]
    fn all_accounted_for_parallel() {
        let amt: HAMT<uint, uint> = HAMT::new();
        let barrier = Arc::new(Barrier::new(num_threads+1));

        for x in range(0, num_threads) {
            let lamt = amt.clone();
            let c = barrier.clone();
            TaskBuilder::new().native().spawn(proc() {
                for i in range((num_elements*x), num_elements*(x+1)) {    
                        lamt.insert(i, i);
                }
                c.wait();
            });
        }
        
        barrier.wait();
        for i in range(0, num_threads*num_elements) {
            if amt.search(i).is_none() {
                fail!("Something went horribly wrong.");
            }
        }
    }

    #[test]
    fn all_removed() {
        let amt: HAMT<uint, uint> = HAMT::new();
        for i in range(0, num_elements) { amt.insert(i, i); };
        for i in range(0, num_elements) { 
            if amt.search(i).is_none() { 
                fail!("Not the first{}", i);
            }
        }
        for i in range(0, num_elements) { 
            if !amt.remove(i) { 
                fail!("{}, ", i);
            }
        }
        for i in range(0, num_elements) { 
            if amt.search(i).is_some() {
                fail!("Fiddle Sticks[{}], ", i);
            }
        }
    }

    #[test]
    fn all_removed_parallel() {
        let amt: HAMT<uint, uint> = HAMT::new();
        let barrier = Arc::new(Barrier::new(num_threads+1));

        for x in range(0, num_threads) {
            let lamt = amt.clone();
            let c = barrier.clone();
            TaskBuilder::new().native().spawn(proc() {
                for i in range((num_elements*x), num_elements*(x+1)) {    
                        lamt.insert(i, i);
                }
                c.wait();
            });
        }
        
        barrier.wait();
        for x in range(0, num_threads) {
            let lamt = amt.clone();
            let c = barrier.clone();
            TaskBuilder::new().native().spawn(proc() {
                for i in range((num_elements*x), num_elements*(x+1)) {    
                        lamt.remove(i);
                }
                c.wait();
            });
        }
        
        barrier.wait();
        for i in range(0, num_threads*num_elements) {
            if amt.search(i).is_some() {
                print!("{}, ", i);
            }
        }
    }

    #[test]
    fn mixed_ins_remove() {
        let amt: HAMT<uint, uint> = HAMT::new();

        for i in range(0, num_elements) { amt.insert(i, i) }
        for i in range(0, num_elements) {
            amt.remove(i);
            amt.insert(i+num_elements, i+num_elements);
        }
        for i in range(0, num_elements) {
            if amt.search(i).is_some() {
                fail!("Oh well..");
            }
            if amt.search(i+num_elements).is_none() {
                fail!("Something bad happened");
            }
        }
    }

    #[test]
    fn mixed_ins_remove_parallel() {
        let amt: HAMT<uint, uint> = HAMT::new();
        let barrier = Arc::new(Barrier::new(num_threads+1));

        for x in range(0, num_threads) {
            let lamt = amt.clone();
            let c = barrier.clone();
            TaskBuilder::new().native().spawn(proc() {
                for i in range((num_elements*x), num_elements*(x+1)) {    
                        lamt.insert(i, i);
                }
                c.wait();
            });
        }
        
        barrier.wait();
        for x in range(0, num_threads) {
            let lamt = amt.clone();
            let c = barrier.clone();
            TaskBuilder::new().native().spawn(proc() {
                for i in range((num_elements*x), num_elements*(x+1)) {    
                        lamt.remove(i);
                        lamt.insert(i+(num_threads*num_elements), i+(num_threads*num_elements));
                }
                c.wait();
            });
        }
        
        barrier.wait();
        for i in range(0, num_threads*num_elements) {
            if amt.search(i).is_some() {
                fail!("Not removed properly");
            }
            if amt.search(i+(num_threads*num_elements)).is_none() {
                fail!("Not inserted properly");
            }
        }
    }
}
