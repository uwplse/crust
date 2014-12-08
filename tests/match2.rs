#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f(x: uint) -> uint {
    match x {
        0 => 1,
        _ => 0,
    }
}

fn crust_init() -> (uint,) { (0,) }
