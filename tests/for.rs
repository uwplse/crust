#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;
extern crate alloc;

use core::prelude::*;


struct FixedIter {
    max: i32,
    curr: i32
}

impl Iterator for FixedIter {
    type Item = i32;

    #[inline]
    fn next(&mut self) -> Option<i32> {
        if self.curr == self.max {
            None
        } else {
            let to_ret = Some(self.curr);
            self.curr += 1;
            to_ret
        }
    }
}


fn do_iter(x: i32) -> i32 {
    let mut iter = FixedIter { curr: 0, max : 4 };
    //iter.size_hint();
    let mut to_ret = 0;
    for i in iter {
        to_ret += i;
    }
    to_ret
}

fn do_iter_temp(x: i32) -> i32 {
    let mut to_ret = 0;
    for i in (FixedIter { curr: 0, max: x }) {
        to_ret += i;
    }
    to_ret
}
