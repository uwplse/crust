#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::ops::Deref;

struct S {
    x: uint,
}

impl Deref for S {
    type Target = uint;
    fn deref(&self) -> &uint {
        &self.x
    }
}

fn get(s: S) -> uint {
    *s
}
