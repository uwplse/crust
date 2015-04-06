#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

use core::prelude::Drop;

fn f() { }

struct S1;

impl Drop for S1 {
    fn drop(&mut self) {
        f();
    }
}

struct S2 {
    x: S1,
    y: S1,
}

fn test() {
    let x = S1;
    {
        let z = S1;
        {
            let s2 = S2 { x: x, y: S1 };
        }
    }
}

fn crust_init() -> (S2,) { (S2 { x: S1, y: S1 }, ) }
