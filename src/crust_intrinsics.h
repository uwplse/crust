rs_u8 nondet_crust_u8();
rs_u16 nondet_crust_u16();
rs_u32 nondet_crust_u32();
rs_u64 nondet_crust_u64();
rs_usize nondet_crust_usize();

rs_i8 nondet_crust_i8();
rs_i16 nondet_crust_i16();
rs_i32 nondet_crust_i32();
rs_i64 nondet_crust_i64();
rs_isize nondet_crust_isize();

rs_char nondet_crust_char();

rs_bool nondet_crust_bool();

rs_f32 nondet_crust_f32();
rs_f64 nondet_crust_f64();

rs_unit crust_assert(int cond) {
  assert(cond);
  return 0;
}
