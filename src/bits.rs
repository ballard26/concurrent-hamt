pub trait Bits {
    fn set(&mut self, loc: usize, val: bool);
    fn get(&self, loc: usize) -> bool;
    fn count(&self, offset: usize) -> usize;
}

impl Bits for usize {
    #[inline(always)]
    fn set(&mut self, loc: usize, val: bool) {
        if val {
            *self = (*self & !(1 << loc)) | (1 << loc);
        } else {
            *self = *self & !(1 << loc);
        }
    }

    #[inline(always)]
    fn get(&self, loc: usize) -> bool {
        if (*self & (1 << loc)) != 0  { true } else { false }
    }

    #[cfg(target_pointer_width = "32")]
    #[inline(always)]
    fn count(&self, offset: usize) -> usize {
        if offset == 32 { 0 } else { ((*self >> offset) as u32).count_ones() as usize }
    }

    #[cfg(target_pointer_width = "64")]
    #[inline(always)]
    fn count(&self, offset: usize) -> usize {
        if offset == 64 { 0 } else { ((*self >> offset) as u64).count_ones() as usize }
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
    fn count_usizev(b: &mut Bencher) {
        let mut uvec: usize = 0;
        uvec.set(0, true);
        uvec.set(1, true);
        uvec.set(20, true);
        uvec.set(31, true);

        b.iter(|| {
            uvec.count(1);
        });
    }

    fn bit_count(bitmap: &Bitv, offset: usize) -> usize {
        bitmap.iter()
              .skip(offset + 1)
              .filter(|x| *x)
              .count()
    }
}
