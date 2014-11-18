#![crate_type = "lib"]

fn foo(x: uint) -> Option<uint> {
    Some(x + 1)
}

fn bar(y: uint) -> Option<uint> {
    None
}
