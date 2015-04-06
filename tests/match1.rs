#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

enum E {
    V1(usize),
    V2(usize, usize),
}

fn unwrap_e(e: E) -> usize {
    match e {
        E::V1(x) => x,
        E::V2(x, y) => x + y,
    }
}

fn crust_init() -> (E,) { (E::V1(0),) }
