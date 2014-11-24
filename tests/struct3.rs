#![crate_type = "lib"]

struct S<'a> {
    x: &'a uint,
}

fn get_x(s: S) -> uint {
    *s.x
}
