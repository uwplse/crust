#![crate_type = "lib"]
#![no_std]
extern crate core;

use core::prelude::Drop;

struct S;
impl Drop for S {
    fn drop(&mut self) { }
}

fn f() -> S {
    S
}

fn g(x: &S) -> bool {
    true
}

fn h() -> bool {
    g(&f())
}
