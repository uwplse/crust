#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: uint,
}

impl S {
    fn get(self, y: uint) -> uint {
        self.x + y
    }
}

fn crust_init() -> (S, uint) { (S { x: 0 }, 0) }
