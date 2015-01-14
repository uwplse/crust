#![crate_type = "lib"]
#![no_std]

extern crate core;

fn foo(z: int) -> uint {
    let x = 4u;
    let y = 5u;
    return match z {
        0 => return x,
        _ => y
    }
}

fn crust_init() -> (int,) { (0i,) }
