#![crate_type = "lib"]
#![no_std]
#![feature(unsafe_destructor)]
extern crate core;

fn f<T: core::cmp::PartialOrd>(x: T, y: T) -> bool {
    x < y
}

fn bar() {
    f(4,5);
}
