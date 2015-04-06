#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f() -> usize {
    let x = true;
    match x {
        true => { 1 },
        false => { 2 },
    }
}

fn g() -> usize {
    let x = true;
    if x { 1 } else { 2 }
}

fn crust_init() -> (usize,) { (0,) }
