#![crate_type = "lib"]

struct S<T> {
    x: T,
}

impl<T> S<T> {
    fn get(self) -> T {
        self.x
    }
}

fn get_x(s: S<uint>) -> uint {
    s.get()
}
