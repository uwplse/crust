#![crate_type = "lib"]
#![no_std]
extern crate core;

const ONE: uint = 1;

fn foo(x: uint) -> uint {
    x + ONE
}

