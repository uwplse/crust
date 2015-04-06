#![feature(no_std)]
#![feature(core)]
#![crate_type = "lib"]

static X: &'static &'static &'static &'static u8 = &&&&0;
