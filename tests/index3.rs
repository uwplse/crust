#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;
use core::ops::{Index, IndexMut};

struct S {
    x: u8,
}

impl Index<usize> for S {
    type Output = u8;
    fn index(&self, index: &usize) -> &u8 {
        &self.x
    }
}

impl IndexMut<usize> for S {
    type Output = u8;
    fn index_mut(&mut self, index: &usize) -> &mut u8 {
        &mut self.x
    }
}

fn test(a: &S) -> &u8 {
    &a[1]
}

fn test_mut(a: &mut S) {
    a[1] = 13;
}
