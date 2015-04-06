#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f(x: usize) -> usize {
    match x {
        0 => 1,
        _ => 0,
    }
}

fn crust_init() -> (usize,) { (0,) }
