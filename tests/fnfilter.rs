#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]

extern crate core;

fn fn_bad(x: isize, f : |isize| -> isize) -> isize {
    f(x)
}

struct BadStruct<A> {
    contents: A
}

impl<T> BadStruct<T> {
    fn bad_map<U>(&self, f: |&T| -> U) -> BadStruct<U> {
        BadStruct { contents: f(&self.contents) }
    }
}

fn fn_good(x: isize) -> isize {
    x + 1
}

fn crust_init() -> (isize,) { (0,) }
