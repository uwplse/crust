#![crate_type = "lib"]

fn f() -> uint {
    let x = true;
    match x {
        true => { 1 },
        false => { 2 },
    }
}

fn g() -> uint {
    let x = true;
    if x { 1 } else { 2 }
}
