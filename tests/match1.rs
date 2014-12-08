#![crate_type = "lib"]
#![no_std]
extern crate core;

enum E {
    V1(uint),
    V2(uint, uint),
}

fn unwrap_e(e: E) -> uint {
    match e {
        E::V1(x) => x,
        E::V2(x, y) => x + y,
    }
}

fn crust_init() -> (E,) { (E::V1(0),) }
