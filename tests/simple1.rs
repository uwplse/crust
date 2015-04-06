#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn foo(x: usize) -> usize {
    let y = x + 1;
    y + 3
}

fn crust_init() -> (usize,) { (0,) }
