#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

use core::prelude::Drop;

struct S;
impl Drop for S {
    fn drop(&mut self) {
    }
}

fn f(x: &S) {
}

fn g() -> S {
    S
}

fn h1() {
    f(&g());
}

fn h2() {
    let s = g();
    f(&s);
}

fn crust_init() -> (S, ) { (S, ) }
