#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::ops::Deref;

struct S {
    x: usize,
}

impl Deref for S {
    type Target = usize;
    fn deref(&self) -> &usize {
        &self.x
    }
}

fn get(s: S) -> usize {
    *s
}

fn crust_init() -> (S,) {
    (S { x: 5 },)
}
