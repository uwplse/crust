#![crate_type = "lib"]

fn foo(x: uint) -> uint {
    let y = x + 1;
    y + 3
}

fn crust_init() -> (uint,) { (0,) }
