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

trait F {
    fn f(&self) -> uint;
}

impl F for S2 {
    fn f(&self) -> uint {
        self.y
    }
}

impl<T> Deref for S<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.x
    }
}

fn get<T: F>(s: &S<T>) -> uint {
    s.f()
}
