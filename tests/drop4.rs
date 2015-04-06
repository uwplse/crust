#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

use core::prelude::Drop;

struct S1;

impl Drop for S1 {
    fn drop(&mut self) {
        f();
    }
}

fn f() -> S1 {
    S1
}

fn test() {
    let x = f();
    f();
}

fn crust_init() -> (S1,) { (S1, ) }
