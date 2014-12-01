#![crate_type = "lib"]

fn f(x: uint) -> uint {
    match x {
        0 => 1,
        _ => 0,
    }
}

fn crust_init() -> (uint,) { (0,) }
