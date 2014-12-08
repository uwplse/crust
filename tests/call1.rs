#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f(x: uint) -> uint {
    x + 1
}

fn g(x: uint) -> uint {
    f(x + 1)
}

fn crust_init() -> (uint,) { (0, ) }
