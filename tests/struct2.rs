#![crate_type = "lib"]

struct S {
    x: uint,
}

fn get_x(s: &S) -> uint {
    s.x
}

fn crust_init() -> (S,) { (S { x: 0 },) }
