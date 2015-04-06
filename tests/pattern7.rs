#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f() -> (u32, u32) {
    (1, 2)
}

fn g() -> u32 {
    let (x, y) = f();
    x
}
