#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S<T> {
    x: T,
}

impl<T> S<T> {
    fn get<U>(self, y: U) -> T {
        self.x
    }
}

fn crust_init() -> (S<uint>, bool) { (S { x: 0 }, true) }
