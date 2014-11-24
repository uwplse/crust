#![crate_type = "lib"]

fn id<T>(x: T) -> T {
    x
}

fn crust_init() -> (uint,) { (0, ) }
