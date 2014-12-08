#![crate_type = "lib"]
#![no_std]
extern crate core;

fn id<T>(x: T) -> T {
    x
}

fn crust_init() -> (uint,) { (0, ) }
