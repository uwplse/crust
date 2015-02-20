extern {
    fn nondet_crust_u8_e() -> u8;
    fn nondet_crust_u16_e() -> u16;
    fn nondet_crust_u32_e() -> u32;
    fn nondet_crust_u64_e() -> u64;

    fn nondet_crust_i8_e() -> i8;
    fn nondet_crust_i16_e() -> i16;
    fn nondet_crust_i32_e() -> i32;
    fn nondet_crust_i64_e() -> i64;

    fn nondet_crust_char_e() -> char;
    
    fn nondet_crust_bool_e() -> bool;

    fn nondet_crust_f32_e() -> f32;
    fn nondet_crust_f64_e() -> f64;
}


fn nondet_crust_u8() -> u8 {
    unsafe {
        nondet_crust_u8_e()
    }
}

fn nondet_crust_u16() -> u16 {
    unsafe {
        nondet_crust_u16_e()
    }
}
fn nondet_crust_u32() -> u32 {
    unsafe {
        nondet_crust_u32_e()
    }
}
fn nondet_crust_u64() -> u64 {
    unsafe {
        nondet_crust_u64_e()
    }
}

fn nondet_crust_i8() -> i8 {
    unsafe {
        nondet_crust_i8_e()
    }
}
fn nondet_crust_i16() -> i16 {
    unsafe {
        nondet_crust_i16_e()
    }
}
fn nondet_crust_i32() -> i32 {
    unsafe {
        nondet_crust_i32_e()
    }
}
fn nondet_crust_i64() -> i64 {
    unsafe {
        nondet_crust_i64_e()
    }
}

fn nondet_crust_char() -> char {
    unsafe {
        nondet_crust_char_e()
    }
}

fn nondet_crust_bool() -> bool {
    unsafe {
        nondet_crust_bool_e()
    }
}

fn nondet_crust_f32() -> f32 {
    unsafe {
        nondet_crust_f32_e()
    }
}
fn nondet_crust_f64() -> f64 {
    unsafe {
        nondet_crust_f64_e()
    }
}

fn crust_assert(_: bool) -> () {
    // this body should be replaced
}
