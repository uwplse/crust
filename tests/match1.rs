#![crate_type = "lib"]

enum E {
    V1(uint),
    V2(uint, uint),
}

fn unwrap_e(e: E) -> uint {
    match e {
        E::V1(x) => x,
        E::V2(x, y) => x + y,
    }
}
