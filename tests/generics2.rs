#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S<T> {
    x: T,
}

impl<T> S<T> {
    fn get(self) -> T {
        self.x
    }
}

fn crust_init() -> (S<uint>,) { (S { x: 0 }, ) }
