#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f(s: &str) { }

fn g() {
    f("hello");
}
