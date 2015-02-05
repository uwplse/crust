#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x : int
}

fn do_thing(x : S) -> int {
    x.x
}

fn crust_init() -> (S,) {
    (S { x : 4 },)
}
