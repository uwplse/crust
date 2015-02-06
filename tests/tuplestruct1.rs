#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;


struct S(u32, u32);

fn f() -> S {
    S(1, 2)
}

fn g(s: S) -> u32 {
    let S(a, _) = s;
    a
}

fn h(s: S) -> u32 {
    s.0
}
