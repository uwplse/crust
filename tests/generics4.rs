#![crate_type = "lib"]

fn id<T>(x: T) -> T {
    x
}

fn user(x: uint) -> uint {
    id(x)
}

fn crust_init() -> (uint,) { (0, ) }
