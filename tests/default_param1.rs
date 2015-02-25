#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;
use core::prelude::*;

mod std {
    pub use core::cmp;
}

#[derive(PartialEq)]
struct S;

impl core::cmp::PartialOrd for S {
    fn partial_cmp(&self, other: &S) -> Option<core::cmp::Ordering> {
        None
    }
}
