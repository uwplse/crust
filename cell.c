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
  ref_cell_t to_ret = { cell_new(int)(0), unsafe_cell_new(T)(value) };
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

ref_mut_t ref_cell_borrow_mut(ref_cell_t *self) {
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

typedef struct {
  int borrowed_from;
  int immutable;
  int type;
  int i;
  int slot;
} stack_info;

/*
 * actions:
 * 0 - pop-stack
 * 1 - new ref-cell
 * 2 - rc_borrow
 * 3 - rc_borrow_mut
 * 4 - r_deref
 * 5 - rm_deref
 * 6 - rm_deref_mut
 */

/* 
 * type codes:
 * 0 - T*
 * 1 - const T*
 * 2 - RefMut<T>
 * 3 - Ref<T>
 * 4 - RefCell<T>
 */

int i_of_src(int slot, int type) {
  if(type == 0) {
	return slot;
  } else if(type == 1) {
	return slot + 2;
  } else if(type == 2) {
	return slot + 4;
  } else if(type == 3) {
	return slot + 7;
  } else if(type == 4) {
	return slot + 10;
  }
}

int main(int argc, char **argv) {
  T val = { 2 };
  ref_cell_t m = ref_cell_new(val);
  int live[11] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  int users[11] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  stack_info stack[10];
  ref_cell_t r_cell[1];
  ref_t refs[3];
  ref_mut_t mut_refs[3];
  T* t_ptr[2];
  const T* const_t_ptr[2];
  int stack_ptr = 0;
  int i;
  for(i = 0; i < 10; i++) {
	int action = nondet_action();
	__CPROVER_assume(action >= 0 && action < 7);
	if(action > 0) {
	  __CPROVER_assume(stack_ptr < 10);
	}
	if(action == 0) {
	  __CPROVER_assume(stack_ptr > 0);
	}
	if(action == 0) { // pop-stack
	  stack_info *to_pop = &stack[stack_ptr-1];
	  int i = stack[stack_ptr-1].i;
	  // this was a borrow
	  if(to_pop->borrowed_from != -1) {
		if(to_pop->immutable) {
		  users[to_pop->borrowed_from]--;
		} else {
		  users[to_pop->borrowed_from] = 0;
		}
	  }
	  assert(users[to_pop->i] == 0);
	  // variable is no longer live
	  live[to_pop->i] = 0;
	  if(to_pop->type == 0 || to_pop->type == 1) {
		// no drop action
	  } else if(to_pop->type == 2) {
		ref_mut_drop(&mut_refs[to_pop->slot]);
	  } else if(to_pop->type == 3) {
		ref_drop(&refs[to_pop->slot]);
	  } else if(to_pop->type == 4) {
		// no drop action for RefCell
	  }
	  stack_ptr--;
	} else if(action == 1) { // new ref-cell
	  int slot = nondet_target();
	  T value = { nondet_target() };
	  __CPROVER_assume(slot >= 0 && slot < 1);
	  int i = i_of_src(slot, 4);
	  __CPROVER_assume(live[i] == 0);
	  r_cell[slot] = ref_cell_new(value);
	  live[i] = 1;
	  stack[stack_ptr].type = 4;
	  stack[stack_ptr].i = i;
	  stack[stack_ptr].slot = slot;
	  stack[stack_ptr].borrowed_from = -1;
	  stack[stack_ptr].immutable = 0;
	  stack_ptr++;
	} else if(action == 2) { // rc_borrow
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < 1);
	  __CPROVER_assume(target_slot >= 0 && target_slot < 3);
	  int src_i = i_of_src(source_slot, 4);
	  int target_i = i_of_src(target_slot, 3);
	  __CPROVER_assume(live[src_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  // this is an "immutable" borrow
	  __CPROVER_assume(users[src_i] != -1);
	  refs[target_slot] = ref_cell_borrow(&r_cell[source_slot]);
	  live[target_i] = 1;
	  stack[stack_ptr].i = target_i;
	  stack[stack_ptr].slot = target_slot;
	  stack[stack_ptr].borrowed_from = src_i;
	  stack[stack_ptr].type = 3;
	  stack[stack_ptr].immutable = 1; // wink wink
	  users[src_i]++;
	  stack_ptr++;
	} else if(action == 3) { // rc_borrow_mut
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < 1);
	  __CPROVER_assume(target_slot >= 0 && target_slot < 3);
	  int src_i = i_of_src(source_slot, 4);
	  int target_i = i_of_src(target_slot, 2);
	  __CPROVER_assume(live[src_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  __CPROVER_assume(users[src_i] != -1);
	  mut_refs[target_slot] = ref_cell_borrow_mut(&r_cell[source_slot]);
	  users[src_i]++;
	  stack[stack_ptr].i = target_i;
	  stack[stack_ptr].slot = target_slot;
	  stack[stack_ptr].borrowed_from = src_i;
	  stack[stack_ptr].immutable = 1;
	  stack[stack_ptr].type = 2;
	  stack_ptr++;
	} else if(action == 4) { // ref deref
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < 3);
	  __CPROVER_assume(target_slot >= 0 && target_slot < 2);
	  int src_i = i_of_src(source_slot, 3);
	  int target_i = i_of_src(target_slot, 1);
	  __CPROVER_assume(live[src_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  __CPROVER_assume(users[src_i] != -1);
	  const_t_ptr[target_slot] = ref_deref(&refs[source_slot]);
	  live[target_i] = 1;
	  users[src_i]++;
	  stack[stack_ptr].i = target_i;
	  stack[stack_ptr].slot = target_slot;
	  stack[stack_ptr].borrowed_from = src_i;
	  stack[stack_ptr].immutable = 1;
	  stack[stack_ptr].type = 1;
	  stack_ptr++;
	} else if(action == 5) { // mut_ref deref
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < 3);
	  __CPROVER_assume(target_slot >= 0 && target_slot < 2);
	  int source_i = i_of_src(source_slot, 2);
	  int target_i = i_of_src(target_slot, 1);
	  __CPROVER_assume(live[source_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  __CPROVER_assume(users[source_i] != -1);
	  const_t_ptr[target_slot] = ref_mut_deref(&mut_refs[source_slot]);
	  live[target_i] = 1;
	  users[source_i]++;
	  stack[stack_ptr].i = target_i;
	  stack[stack_ptr].slot = target_slot;
	  stack[stack_ptr].borrowed_from = source_i;
	  stack[stack_ptr].immutable = 1;
	  stack[stack_ptr].type = 1;
	  stack_ptr++;
	} else if(action == 6) { // mut_ref deref_mut
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < 3);
	  __CPROVER_assume(target_slot >= 0 && target_slot < 2);
	  int source_i = i_of_src(source_slot, 2);
	  int target_i = i_of_src(target_slot, 0);
	  __CPROVER_assume(live[source_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  __CPROVER_assume(users[source_i] == 0);
	  t_ptr[target_slot] = ref_mut_deref_mut(&mut_refs[source_slot]);
	  live[target_i] = 1;
	  users[source_i] = -1;
	  stack[stack_ptr].i = target_i;
	  stack[stack_ptr].slot = target_slot;
	  stack[stack_ptr].borrowed_from = source_i;
	  stack[stack_ptr].immutable = 0;
	  stack[stack_ptr].type = 0;
	  stack_ptr++;
	}
	for(int j = 0; j < 2; j++) {
	  for(int k = 0; k < 2; k++) {
		assert(live[i_of_src(j, 0)] == 0 || live[i_of_src(k, 1)] || t_ptr[i_of_src(j, 0)] != const_t_ptr[i_of_src(k, 1)]);
		if(j == k) { continue; }
		assert(live[i_of_src(j, 0)] == 0 || live[i_of_src(k, 0)] || t_ptr[i_of_src(j, 0)] != t_ptr[i_of_src(k, 0)]);
	  }
	}
  }
  return 0;
}

/*
 * infer mut borrow from lifetimes and whether the ARUGMENT reference type is mutable
 * raise an error if lifetime arguments appear in some other context than rerference types
 */
