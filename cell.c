#include<stdlib.h>
#include<assert.h>
#define unsafe_cell_t(type) unsafe_cell_##type##_t
#define option_t(type) option_##type##_t
#define option_t_impl(type) typedef struct { option_flag_t flag; type maybe; } option_t(type);
#define unsafe_cell_new(type) Unsafe_Cell_##type##_new
#define unsafe_cell_unwrap(type) Unsafe_Cell_##type##_unwrap
#define unsafe_cell_get(type) Unsafe_Cell_##type##_get
#define unsafe_cell_t_impl(type) typedef struct { type value; } unsafe_cell_t(type); \
  unsafe_cell_t(type) unsafe_cell_new(type)(type t) {					\
	unsafe_cell_t(type) to_ret;											\
	to_ret.value = t;													\
  }																		\
  type *unsafe_cell_get(type)(unsafe_cell_t(type) *self) {				\
	return &self->value;												\
  }																		\
  type unsafe_cell_unwrap(type)(unsafe_cell_t(type) *self) {			\
	return self->value;													\
  }
	
#define cell_t(type) cell_##type##_t
#define cell_new(type) Cell_##type##_new
#define cell_get(type) Cell_##type##_get
#define cell_set(type) Cell_##type##_set
#define cell_t_impl(type) typedef struct { unsafe_cell_t(type) value; } cell_t(type); \
  cell_t(type) cell_new(type)(type t) {									\
	cell_t(type) to_ret;												\
	to_ret.value = unsafe_cell_new(type)(t);							\
	return to_ret;														\
  }																		\
  type cell_get(type)(cell_t(type) *self) {								\
	return *(unsafe_cell_get(type)(&self->value));						\
  }																		\
  void cell_set(type)(cell_t(type) *self, type value) {					\
	*(unsafe_cell_get(type)(&self->value)) = value;						\
  }
//#define __CPROVER_assume(x)

typedef struct {
  int a;
} T;

unsafe_cell_t_impl(T)
unsafe_cell_t_impl(int);
cell_t_impl(int);

typedef enum {
  SOME,
  NONE
} option_flag_t;

typedef struct ref_cell {
  cell_t(int) borrow;
  unsafe_cell_t(T) value;
} ref_cell_t;

typedef struct {
  ref_cell_t *parent;
} ref_mut_t;

option_t_impl(ref_mut_t)

typedef struct {
  ref_cell_t *parent;
} ref_t;

option_t_impl(ref_t)

ref_cell_t ref_cell_new(T value) {
  ref_cell_t to_ret = { 0, unsafe_cell_new(T)(value) };
  return to_ret;
}

T ref_cell_unwrap(ref_cell_t *self) {
  return unsafe_cell_unwrap(T)(&self->value);
}

option_t(ref_t) ref_cell_try_borrow(ref_cell_t *self) {
  int _temp = cell_get(int)(&self->borrow);
  if(_temp == -1) {
	option_t(ref_t) to_ret;
	to_ret.flag = NONE;
	return to_ret;
  } else {
	option_t(ref_t) to_ret;
	cell_set(int)(&self->borrow, _temp + 1);
	to_ret.maybe.parent = self;
	return to_ret;
  }
}

option_t(ref_mut_t) ref_cell_try_mut_borrow(ref_cell_t *self) {
  int _temp = cell_get(int)(&self->borrow);
  if(_temp == 0) {
	option_t(ref_mut_t) to_ret;
	to_ret.flag = SOME;
	to_ret.maybe.parent = self;
	cell_set(int)(&self->borrow,-1);
	return to_ret;
  } else {
	option_t(ref_mut_t) to_ret;
	to_ret.flag = NONE;
	return to_ret;
  }
}

ref_mut_t ref_cell_mut_borrow(ref_cell_t *self) {
  option_t(ref_mut_t) _temp = ref_cell_try_mut_borrow(self);
  if(_temp.flag == NONE) {
	__CPROVER_assume(1 == 0);
  } else {
	return _temp.maybe;
  }
}

ref_t ref_cell_borrow(ref_cell_t *self) {
  option_t(ref_t) _temp = ref_cell_try_borrow(self);
  if(_temp.flag == NONE) {
	__CPROVER_assume(1 == 0);
  } else {
	return _temp.maybe;
  }
}

void ref_drop(ref_t *self) {
  cell_set(int)(&self->parent->borrow, cell_get(int)(&self->parent->borrow) - 1);
}

const T *ref_deref(ref_t *self) {
  return unsafe_cell_get(T)(&self->parent->value);
}

void ref_mut_drop(ref_mut_t *self) {
  cell_set(int)(&self->parent->borrow, 0);
}

T *ref_mut_deref_mut(ref_mut_t *self) {
  return unsafe_cell_get(T)(&self->parent->value);
}

const T *ref_mut_deref(ref_mut_t *self) {
  return unsafe_cell_get(T)(&self->parent->value);
}

short nondet_action();
short nondet_target();

int main(int argc, char **argv) {
  T val = { 2 };
  ref_cell_t m = ref_cell_new(val);
  int m_flags[6] = { 0, 0, 0, 0, 0, 0 };
  int im_flags[6] = { 0, 0, 0, 0, 0, 0 };
  ref_mut_t mut_refs[6];
  ref_t refs[6];
  int i;
  for(i = 0; i < 10; i++) {
	int action = nondet_action();
	__CPROVER_assume(action >= 0 && action < 4);
	int target = nondet_target();
	__CPROVER_assume(target >= 0 && target < 6);
	if(action == 0) { // take mutable reference
	  __CPROVER_assume(m_flags[target] == 0);
	  mut_refs[target] = ref_cell_mut_borrow(&m);
	  m_flags[target] = 1;
	} else if(action == 1) { // drop mutable reference
	  __CPROVER_assume(m_flags[target] == 1);
	  m_flags[target] = 0;
	  ref_mut_drop(&mut_refs[target]);
	} else if(action == 2) { // take immutable reference
	  __CPROVER_assume(im_flags[target] == 0);
	  refs[target] = ref_cell_borrow(&m);
	  im_flags[target] = 1;
	} else if(action == 3) { // drop immutable reference
	  __CPROVER_assume(im_flags[target] == 1);
	  ref_drop(&refs[target]);
	  im_flags[target] = 0;
	}
	for(int j = 0; j < 6; j++) {
	  for(int k = 0; k < 6; k++) {
		if(j == k) { continue; }
		assert(m_flags[j] == 0 || m_flags[k] == 0 || mut_ref_mut_deref(&mut_refs[j]) != mut_ref_mut_deref(&mut_refs[k]));
		assert(m_flags[j] == 0 || im_flags[k] == 0 || mut_ref_mut_deref(&mut_refs[j]) != ref_deref(&refs[k]));
	  }
	}
  }
  return 0;
}

/*
 * infer mut borrow from lifetimes and whether the ARUGMENT reference type is mutable
 * raise an error if lifetime arguments appear in some other context than rerference types
 */
