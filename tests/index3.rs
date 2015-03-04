#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;
use core::ops::{Index, IndexMut};

struct S {
    x: u8,
}

impl Index<uint> for S {
    type Output = u8;
    fn index(&self, index: &uint) -> &u8 {
        &self.x
    }
}

impl IndexMut<uint> for S {
    type Output = u8;
    fn index_mut(&mut self, index: &uint) -> &mut u8 {
        &mut self.x
    }
}

fn test(a: &S) -> &u8 {
    &a[1]
}

fn test_mut(a: &mut S) {
    a[1] = 13;
}
