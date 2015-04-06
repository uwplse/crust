#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::prelude::{Option, None, Some};

fn foo(x: usize) -> Option<usize> {
    Some(x + 1)
}

fn bar(y: usize) -> Option<usize> {
    None
}

fn crust_init() -> (usize,) { (0, ) }
