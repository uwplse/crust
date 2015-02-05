#![crate_type = "lib"]
#![no_std]
extern crate core;

enum Foo {
    Bar(uint),
    Baz
}

fn frob(x : uint) -> Foo {
    if x == 3 {
        Foo::Baz
    } else {
        Foo::Bar(x)
    }
}

fn g(mut x: uint) -> uint {
    let mut y = 0;
    while match frob(x) {
        Foo::Baz => false,
        Foo::Bar(x) => x > 0
    } {
        x -= 1;
        y += 1;
    }
    y
}
