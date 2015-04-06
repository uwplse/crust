#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: usize,
}

impl S {
    fn get(self) -> usize {
        self.x
    }
}

fn get_x(s: S) -> usize {
    s.get()
}

fn crust_init() -> (S,) { (S { x: 0 },) }
