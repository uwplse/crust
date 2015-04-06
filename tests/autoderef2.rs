#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::ops::Deref;

struct S {
    x: S2,
}

struct S2 {
    y: usize,
}

impl Deref for S {
    type Target = S2;
    fn deref(&self) -> &S2 {
        &self.x
    }
}

fn get(s: S) -> usize {
    s.y
}

fn crust_init() -> (S,) {
    (S { x : S2 { y : 32 }},)
}
