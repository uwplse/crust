#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

use core::prelude::Drop;


struct S;

impl Drop for S {
    fn drop(&mut self) { }
}


enum Option<T> {
    Some(T),
    None,
}

fn f() {
    let x = Option::Some(S);
}
