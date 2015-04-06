#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: usize,
}

impl S {
    fn get(self, y: usize) -> usize {
        self.x + y
    }
}

fn crust_init() -> (S, usize) { (S { x: 0 }, 0) }
