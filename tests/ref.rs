#![crate_type = "lib"]
#![no_std]
extern crate core;

struct S {
    x: int
}

impl S {
    fn do_thing(x: &int) -> int{
        *x + 1
    }
    
}

fn whatever(y: int, x : (int,(uint,&int))) -> &int {
    match x {
        (_,(_,p)) => p
    }
}

struct Cont<T> {
    contents: T
}

impl<T> Cont<T> {
    fn get_mut_ref<'a>(&'a mut self) -> &'a mut T {
        &mut self.contents
    }
    fn get_ref(&self) -> &T {
        &self.contents
    }
}

fn crust_init () -> (S,Cont<int>) {
    (S { x : 4 },Cont { contents: 4 })
}
