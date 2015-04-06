#![feature(no_std)]
#![feature(core)]
// Known broken: no handling of statics yet.
#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S<'a> {
    x: &'a usize,
}

fn get_x(s: S) -> usize {
    *s.x
}

static ZERO: usize = 0;
fn crust_init() -> (S<'static>,) { (S { x: &ZERO },) }
