#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::prelude::{Option, None, Some};

fn foo(x: uint) -> Option<uint> {
    Some(x + 1)
}

fn bar(y: uint) -> Option<uint> {
    None
}

fn crust_init() -> (uint,) { (0, ) }
