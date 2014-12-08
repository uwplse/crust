#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: uint,
}

impl S {
    fn test<'a, 'b>(&'a self, a: &'b uint, b: &uint) -> &'a uint {
        &self.x
    }
}

fn crust_init() -> (S,) { (S { x: 0 },) }
