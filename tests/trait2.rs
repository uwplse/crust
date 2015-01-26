#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::ops::Add;

struct S {
    x: uint,
}

trait T {
    fn f(&self) -> uint;
}

impl T for S {
    fn f(&self) -> uint {
        self.x
    }
}

fn g<A: T>(x: A) -> uint {
    x.f()
}

fn crust_init() -> (S,) { (S { x: 0 },) }
