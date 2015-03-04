#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::ops::Deref;

struct S {
    x: S2,
}

struct S2 {
    y: uint,
}

impl Deref for S {
    type Target = S2;
    fn deref(&self) -> &S2 {
        &self.x
    }
}

fn get(s: S) -> uint {
    s.y
}
