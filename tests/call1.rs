#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f(x: usize) -> usize {
    x + 1
}

fn g(x: usize) -> usize {
    f(x + 1)
}

fn crust_init() -> (usize,) { (0, ) }
