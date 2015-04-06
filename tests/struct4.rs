#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![feature(unsafe_destructor)]
#![no_std]
extern crate core;
use core::prelude::Drop;

struct S<'a> {
    p: &'a mut usize,
}

impl<'a> S<'a> {
    fn new(p: &'a mut usize) -> S<'a> { S { p: p } }
}

#[unsafe_destructor]
impl<'a> Drop for S<'a> {
    fn drop(&mut self) {
        *self.p = 3;
    }
}

fn crust_init() -> (usize,) { (2,) }
