#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

enum E {
    V1(usize),
    V2(usize, usize),
}

fn mk_e(x: usize) -> E {
    E::V2(x, x)
}

fn crust_init() -> (usize,) { (0, ) }
