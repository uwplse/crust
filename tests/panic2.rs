#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
#[macro_use] extern crate core;

mod std {
    pub use core::fmt;
}


fn die() {
    panic!("oh {}", "no");
}
