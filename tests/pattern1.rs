#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f(x: (u32, u32)) -> u32 {
    let (a, b) = x;
    a + b
}
