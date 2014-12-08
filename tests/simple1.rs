#![crate_type = "lib"]
#![no_std]
extern crate core;

fn foo(x: uint) -> uint {
    let y = x + 1;
    y + 3
}

fn crust_init() -> (uint,) { (0,) }
