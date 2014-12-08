#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S<'a> {
    x: &'a uint,
}

fn get_x(s: S) -> uint {
    *s.x
}

static ZERO: uint = 0;
fn crust_init() -> (S<'static>,) { (S { x: &ZERO },) }
