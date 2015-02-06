#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f((a, b): (u32, u32)) -> u32 {
    a + b
}
