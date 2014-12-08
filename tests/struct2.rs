#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: uint,
}

fn get_x(s: &S) -> uint {
    s.x
}

fn crust_init() -> (S,) { (S { x: 0 },) }
