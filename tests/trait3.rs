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

impl Add<S> for S {
    type Output = S;
    fn add(self, other: S) -> S {
        S { x: self.x + other.x }
    }
}

fn g<A: Add<A>>(x: A, y: A) {
    x + y;
}

fn crust_init() -> (S,) { (S { x: 0 },) }
