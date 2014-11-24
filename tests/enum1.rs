#![crate_type = "lib"]

enum E {
    V1(uint),
    V2(uint, uint),
}

fn mk_e(x: uint) -> E {
    E::V2(x, x)
}

fn crust_init() -> (uint,) { (0, ) }
