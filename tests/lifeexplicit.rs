#![crate_type = "lib"]

struct S {
    x: uint,
}

impl S {
    fn test<'a, 'b>(&'a self, a: &'b uint, b: &uint) -> &'a uint {
        &self.x
    }
}
