#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;
use core::ops::Index;

struct S {
    x: u8,
}

impl Index<usize> for S {
    type Output = u8;
    fn index(&self, index: &usize) -> &u8 {
        &self.x
    }
}

fn test(a: S) -> u8 {
    a[1]
}
