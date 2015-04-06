#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;

mod std {
    pub use core::cmp;
    pub use core::option;
}

#[derive(PartialEq, PartialOrd)]
struct S;

fn f() -> bool {
    S < S
}
