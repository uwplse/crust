#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f(x: &mut (u32, u32)) {
    let (ref mut a, _) = *x;
    *a = 3;
}
