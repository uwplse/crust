#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::ops::Deref;

struct S<T> {
    x: T,
}

struct S2 {
    y: uint,
}

impl<T> Deref for S<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.x
    }
}

fn get(s: S<S2>) -> uint {
    s.y
}
