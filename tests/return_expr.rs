#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]

extern crate core;

fn test(x: bool) {
    if x {
        return
    }
}

fn test2() {
    let x = return;
    x
}

fn test3() {
    return
}
