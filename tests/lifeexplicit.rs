#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: usize,
}

impl S {
    fn test<'a, 'b>(&'a self, a: &'b usize, b: &usize) -> &'a usize {
        &self.x
    }
}

fn crust_init() -> (S,) { (S { x: 0 },) }
