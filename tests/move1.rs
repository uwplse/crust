#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x : isize
}

fn do_thing(x : S) -> isize {
    x.x
}

fn crust_init() -> (S,) {
    (S { x : 4 },)
}
