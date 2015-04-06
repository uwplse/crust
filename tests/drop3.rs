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

fn drop(x: S1) { }

fn test() {
    let x = S1;

    if true {
        drop(x);
    }
}

fn crust_init() -> (S1,) { (S1, ) }
