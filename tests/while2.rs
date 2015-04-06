#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

enum Foo {
    Bar(usize),
    Baz
}

fn frob(x : usize) -> Foo {
    if x == 3 {
        Foo::Baz
    } else {
        Foo::Bar(x)
    }
}

fn g(mut x: usize) -> usize {
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
