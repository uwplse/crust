#![crate_type = "lib"]
#![no_std]
extern crate core;
use core::ops::Add;

enum MyOption<T> {
    Some(T),
    None
}

trait Foo<T> {
    type Output;
    fn do_foo(self, y: T) -> Self::Output;    
}

impl Foo<i32> for i32 {
    type Output = i32;
    fn do_foo(self, y: i32) -> i32 {
        self + y
    }
}

impl Foo<i64> for i64 {
    type Output = i64;
    fn do_foo(self, y: i64) -> i64 {
        self + y
    }
}

impl<T: Foo<T>> Foo<MyOption<T>> for MyOption<T> {
    type Output = MyOption<<T as Foo<T>>::Output>;
    fn do_foo(self, y: MyOption<T>) -> MyOption<<T as Foo<T>>::Output> {
        match self {
            MyOption::None => MyOption::None,
            MyOption::Some(x_) => match y {
                MyOption::None => MyOption::None,
                MyOption::Some(y_) => MyOption::Some(x_.do_foo(y_))
            }
        }
    }
}

fn baz(f: <i32 as Foo<i32>>::Output) -> i32 {
    f
}

fn gorp<T, U: Foo<T>>(f: <U as Foo<T>>::Output) -> <U as Foo<T>>::Output {
    f
}

fn bar<U, T: Foo<U>>(f: T, x: U, y: <T as Foo<U>>::Output) -> <T as Foo<U>>::Output {
    f.do_foo(x)
}

fn qux(f : <i64 as Foo<i64>>::Output) -> bool {
    true
}

fn foo1<T: Foo<T>>(f: <MyOption<T> as Foo<MyOption<T>>>::Output) -> bool {
    true
}

fn x() -> i32 {
    let x : i32 = 66;
    gorp::<i32, i32>(x)
}

fn y() -> i32 {
    bar(3, 4, 5)
}

fn crust_init() -> (MyOption<i32>,) {
    (MyOption::Some(33),)
}
