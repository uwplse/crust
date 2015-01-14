#![crate_type = "lib"]
#![no_std]
extern crate core;

fn f(x: uint) -> uint {
    match x {
        0 => 1,
        _ => 0,
    }
}

struct Foo {
    x: uint
}

fn g(x: uint) -> Foo {
    let z = Foo { x : 4u };
    let y = Foo { x : 5u };
    let a = {if x == 0 {
        z
    } else {
        y
    }};
    return a
}

fn crust_init() -> (uint,) { (0,) }
