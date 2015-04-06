#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f() -> u32 {
    3
}

fn g(x: &u32) -> bool {
    true
}

fn h() -> bool {
    g(&2);
    g(&f())
}
