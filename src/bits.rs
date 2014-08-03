pub trait Bits {
    fn set(&mut self, loc: uint, val: bool);
    fn get(&self, loc: uint) -> bool;
    fn count(&self, offset: uint) -> uint;
}

impl Bits for uint {
    #[inline(always)]
    fn set(&mut self, loc: uint, val: bool) {
        if val {
            *self = (*self & !(1 << loc)) | (1 << loc);
        } else {
            *self = *self & !(1 << loc);
        }
    }
    
    #[inline(always)]
    fn get(&self, loc: uint) -> bool {
        if (*self & (1 << loc)) != 0  { true } else { false }
    }

    #[cfg(target_word_size = "32")]
    #[inline(always)]
    fn count(&self, offset: uint) -> uint {
        unsafe { 
            use std::intrinsics::ctpop32;
            if offset == 32 { 0 } else { ctpop32((*self >> offset) as u32) as uint }
        }
    }

    #[cfg(target_word_size = "64")]
    #[inline(always)]
    fn count(&self, offset: uint) -> uint {
        unsafe { 
            use std::intrinsics::ctpop64;
            if offset == 64 { 0 } else { ctpop64((*self >> offset) as u64) as uint }
        }
    }
}

#[cfg(bench)]
mod bench {
    extern crate test;

    use self::test::Bencher;
    use std::collections::bitv::Bitv;
    use super::Bits;    

    #[bench]
    fn count_bitv(b: &mut Bencher) {
        let mut bvec: Bitv = Bitv::with_capacity(32, false);
        bvec.set(0, true);
        bvec.set(1, true);
        bvec.set(20, true);
        bvec.set(31, true);

        b.iter(|| {
            bit_count(&bvec, 0);
        });
    }

    #[bench]
    fn count_uintv(b: &mut Bencher) {
        let mut uvec: uint = 0;
        uvec.set(0, true);
        uvec.set(1, true);
        uvec.set(20, true);
        uvec.set(31, true);

        b.iter(|| {
            uvec.count(1);
        });
    }

    fn bit_count(bitmap: &Bitv, offset: uint) -> uint {
        bitmap.iter()
              .skip(offset + 1u)
              .filter(|x| *x)
              .count()
    }
}
