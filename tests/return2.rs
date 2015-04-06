#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]

extern crate core;

struct Foo { x : isize }

fn foo(z: isize) -> Foo {
    let a = Foo { x : 4i };
    Foo {
        x : return a
    }
}

fn crust_init() -> (isize,) { (0i,) }
