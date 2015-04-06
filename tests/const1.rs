#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

const ONE: usize = 1;

fn foo(x: usize) -> usize {
    x + ONE
}

