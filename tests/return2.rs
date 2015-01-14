#![crate_type = "lib"]
#![no_std]

extern crate core;

struct Foo { x : int }

fn foo(z: int) -> Foo {
    let a = Foo { x : 4i };
    Foo {
        x : return a
    }
}

fn crust_init() -> (int,) { (0i,) }
