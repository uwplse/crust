#![crate_type = "lib"]

struct S<T> {
    x: T,
}

impl<T> S<T> {
    fn get(self) -> T {
        self.x
    }
}