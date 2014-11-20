#![crate_type = "lib"]

struct S<T> {
    x: T,
}

impl<T> S<T> {
    fn get<U>(self, y: U) -> T {
        self.x
    }
}
