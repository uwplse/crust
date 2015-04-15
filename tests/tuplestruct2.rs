#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;

struct S(u32);

fn f(s: &S) -> &u32 {
    let S(ref x) = *s;
    x
}

fn g() -> S{
    S(17)
}

fn f2() -> u32 {
    let S(ref x) = g();
    *x
}

