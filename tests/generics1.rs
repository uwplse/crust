#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn id<T>(x: T) -> T {
    x
}

fn crust_init() -> (usize,) { (0, ) }
