#![crate_name = "simplert"]
#![crate_type = "lib"]
#![no_std]

#![feature(lang_items)]

#![allow(unstable)] // much of libcore is unstable as of Rust 1.0

#[macro_use] extern crate core;

use core::prelude::*;
use core::fmt;


// Essential lang items.  These would normally be provided by librustrt.

#[inline(always)] #[cold]
#[lang = "panic_fmt"]
extern fn lang_panic_fmt(args: &core::fmt::Arguments,
                         file: &'static str,
                         line: usize) -> ! {
    unsafe { core::intrinsics::abort() };
}

#[inline(always)] #[cold]
#[lang = "stack_exhausted"]
extern fn lang_stack_exhausted() -> ! {
    unsafe { core::intrinsics::abort() };
}

#[inline(always)] #[cold]
#[lang = "eh_personality"]
extern fn lang_eh_personality() -> ! {
    unsafe { core::intrinsics::abort() };
}
