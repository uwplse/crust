#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]
#![no_std]
extern crate core;

static FOO : u32 = 5;
static BAZ : u32 = 4;
static BAR : [&'static u32; 2] = [&BAZ, &FOO];

static GORP : & 'static [& 'static u32; 2] = &BAR;

fn foo() {
    let x = *GORP;
}
