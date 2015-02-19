extern {
    fn crust_nondet_u8_e() -> u8;
    fn crust_nondet_u16_e() -> u16;
    fn crust_nondet_u32_e() -> u32;
    fn crust_nondet_u64_e() -> u64;

    fn crust_nondet_i8_e() -> i8;
    fn crust_nondet_i16_e() -> i16;
    fn crust_nondet_i32_e() -> i32;
    fn crust_nondet_i64_e() -> i64;

    fn crust_nondet_char_e() -> char;
    
    fn crust_nondet_bool_e() -> bool;

    fn crust_nondet_f32_e() -> f32;
    fn crust_nondet_f64_e() -> f64;
}


fn crust_nondet_u8() -> u8 {
    unsafe {
        crust_nondet_u8_e()
    }
}

fn crust_nondet_u16() -> u16 {
    unsafe {
        crust_nondet_u16_e()
    }
}
fn crust_nondet_u32() -> u32 {
    unsafe {
        crust_nondet_u32_e()
    }
}
fn crust_nondet_u64() -> u64 {
    unsafe {
        crust_nondet_u64_e()
    }
}

fn crust_nondet_i8() -> i8 {
    unsafe {
        crust_nondet_i8_e()
    }
}
fn crust_nondet_i16() -> i16 {
    unsafe {
        crust_nondet_i16_e()
    }
}
fn crust_nondet_i32() -> i32 {
    unsafe {
        crust_nondet_i32_e()
    }
}
fn crust_nondet_i64() -> i64 {
    unsafe {
        crust_nondet_i64_e()
    }
}

fn crust_nondet_char() -> char {
    unsafe {
        crust_nondet_char_e()
    }
}

fn crust_nondet_bool() -> bool {
    unsafe {
        crust_nondet_bool_e()
    }
}

fn crust_nondet_f32() -> f32 {
    unsafe {
        crust_nondet_f32_e()
    }
}
fn crust_nondet_f64() -> f64 {
    unsafe {
        crust_nondet_f64_e()
    }
}
