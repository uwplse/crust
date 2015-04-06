#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: usize,
}

fn get_x(s: S) -> usize {
    s.x
}

fn crust_init() -> (S,) { (S { x: 0 },) }
