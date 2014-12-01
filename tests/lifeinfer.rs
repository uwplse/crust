#![crate_type = "lib"]

struct S {
    x: uint,
}

impl S {
    fn test(&self, a: &uint, b: &uint) -> &uint {
        &self.x
    }
}
