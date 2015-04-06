#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: usize,
}

impl S {
    fn test(&self, a: &usize, b: &usize) -> &usize {
        &self.x
    }
}

fn crust_init() -> (S, usize) { (S { x: 0 }, 0) }
