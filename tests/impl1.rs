#![crate_type = "lib"]

struct S {
    x: uint,
}

impl S {
    fn get(self, y: uint) -> uint {
        self.x + y
    }
}
