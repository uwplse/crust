#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]

extern crate core;

fn foo(z: isize) -> usize {
    let x = 4u;
    let y = 5u;
    return match z {
        0 => return x,
        _ => y
    }
}

fn crust_init() -> (isize,) { (0i,) }
