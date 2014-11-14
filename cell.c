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
	return to_ret;														\
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

#define update_stack(_i, _slot, _borrowed_from, _immutable, _type) \
  stack[stack_ptr].i = _i;									  \
  stack[stack_ptr].slot = _slot;							  \
  stack[stack_ptr].borrowed_from = _borrowed_from;			  \
  stack[stack_ptr].immutable = _immutable;					  \
  stack[stack_ptr].type = _type;							  \
  stack_ptr++;

#define IMMUTABLE 1
#define MUTABLE 0
#define NO_BORROW -1

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

// type codes
#define TYPE_MUT_PTR 0 /* T* */
#define TYPE_CONST_PTR 1 /* const T* */
#define TYPE_REF_MUT 2 /* RefMut<T> */
#define TYPE_REF 3 /* Ref<T> */
#define TYPE_REF_CELL 4 /* RefCell<T> */

/*
 * System parameters
 */
#define N_MUT_PTR 2
#define N_CONST_PTR N_MUT_PTR
#define N_REF_MUT 3
#define N_REF 3
#define N_REF_CELL 2
#define N_OBJECTS (N_MUT_PTR + N_CONST_PTR + N_REF_MUT + N_REF + N_REF_CELL)

#define MAX_STACK 10
#define MAX_ACTIONS 10

int i_of_src(int slot, int type) {
  if(type == TYPE_MUT_PTR) {
	return slot;
  }
  slot += N_MUT_PTR;
  if(type == TYPE_CONST_PTR) {
	return slot;
  }
  slot += N_CONST_PTR;
  if(type == TYPE_REF_MUT) {
	return slot;
  }
  slot += N_REF_MUT;
  if(type == TYPE_REF) {
	return slot;
  }
  slot += N_REF;
  if(type == TYPE_REF_CELL) {
	return slot;
  }
  assert(0);
  return -1;
}

int main(int argc, char **argv) {
  int live[N_OBJECTS];
  {
	for(int l = 0; l < N_OBJECTS; l++) {
	  live[l] = 0;
	}
  }
  int users[N_OBJECTS];
  {
	for(int l = 0; l < N_OBJECTS; l++) {
	  users[l] = 0;
	}
  }
  stack_info stack[MAX_STACK];
  ref_cell_t r_cell[N_REF_CELL];
  ref_t refs[N_REF];
  ref_mut_t mut_refs[N_REF_MUT];
  T* t_ptr[N_MUT_PTR];
  const T* const_t_ptr[N_CONST_PTR];
  int stack_ptr = 0;
  int i;
  for(i = 0; i < MAX_ACTIONS; i++) {
	int action = nondet_action();
	__CPROVER_assume(action >= 0 && action < 7);
	if(action > 0) {
	  __CPROVER_assume(stack_ptr < MAX_STACK);
	}
	if(action == 0) {
	  __CPROVER_assume(stack_ptr > 0);
	}
	if(action == 0) { // pop-stack
	  stack_info *to_pop = &stack[stack_ptr-1];
	  // this was a borrow
	  if(to_pop->borrowed_from != NO_BORROW) {
		if(to_pop->immutable == IMMUTABLE) {
		  users[to_pop->borrowed_from]--;
		} else {
		  users[to_pop->borrowed_from] = 0;
		}
	  }
	  assert(users[to_pop->i] == 0);
	  // variable is no longer live
	  live[to_pop->i] = 0;
	  if(to_pop->type == TYPE_MUT_PTR || to_pop->type == TYPE_CONST_PTR) {
		// no drop action
	  } else if(to_pop->type == TYPE_REF_MUT) {
		ref_mut_drop(&mut_refs[to_pop->slot]);
	  } else if(to_pop->type == TYPE_REF) {
		ref_drop(&refs[to_pop->slot]);
	  } else if(to_pop->type == TYPE_REF_CELL) {
		// no drop action for RefCell
	  }
	  stack_ptr--;
	} else if(action == 1) { // new ref-cell
	  int slot = nondet_target();
	  T value = { nondet_target() };
	  __CPROVER_assume(slot >= 0 && slot < N_REF_CELL);
	  int target_i = i_of_src(slot, TYPE_REF_CELL);
	  __CPROVER_assume(live[target_i] == 0);
	  r_cell[slot] = ref_cell_new(value);
	  live[target_i] = 1;
	  update_stack(target_i, slot, NO_BORROW, NO_BORROW, TYPE_REF_CELL);
	} else if(action == 2) { // rc_borrow
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < N_REF_CELL);
	  __CPROVER_assume(target_slot >= 0 && target_slot < N_REF);
	  int src_i = i_of_src(source_slot, TYPE_REF_CELL);
	  int target_i = i_of_src(target_slot, TYPE_REF);
	  __CPROVER_assume(live[src_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  // this is an "immutable" borrow
	  __CPROVER_assume(users[src_i] != -1);
	  refs[target_slot] = ref_cell_borrow(&r_cell[source_slot]);
	  live[target_i] = 1;
	  update_stack(target_i, target_slot, src_i, IMMUTABLE, TYPE_REF);
	  users[src_i]++;
	} else if(action == 3) { // rc_borrow_mut
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < N_REF_CELL);
	  __CPROVER_assume(target_slot >= 0 && target_slot < N_REF_MUT);
	  int src_i = i_of_src(source_slot, TYPE_REF_CELL);
	  int target_i = i_of_src(target_slot, TYPE_REF_MUT);
	  __CPROVER_assume(live[src_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  __CPROVER_assume(users[src_i] != -1);
	  mut_refs[target_slot] = ref_cell_borrow_mut(&r_cell[source_slot]);
	  users[src_i]++;
	  update_stack(target_i, target_slot, src_i, IMMUTABLE, TYPE_REF);
	} else if(action == 4) { // ref deref
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < N_REF);
	  __CPROVER_assume(target_slot >= 0 && target_slot < N_CONST_PTR);
	  int src_i = i_of_src(source_slot, TYPE_REF);
	  int target_i = i_of_src(target_slot, TYPE_CONST_PTR);
	  __CPROVER_assume(live[src_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  __CPROVER_assume(users[src_i] != -1);
	  const_t_ptr[target_slot] = ref_deref(&refs[source_slot]);
	  live[target_i] = 1;
	  users[src_i]++;
	  update_stack(target_i, target_slot, src_i, IMMUTABLE, TYPE_CONST_PTR);
	} else if(action == 5) { // mut_ref deref
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < N_REF_MUT);
	  __CPROVER_assume(target_slot >= 0 && target_slot < N_CONST_PTR);
	  int source_i = i_of_src(source_slot, TYPE_REF_MUT);
	  int target_i = i_of_src(target_slot, TYPE_CONST_PTR);
	  __CPROVER_assume(live[source_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  __CPROVER_assume(users[source_i] != -1);
	  const_t_ptr[target_slot] = ref_mut_deref(&mut_refs[source_slot]);
	  live[target_i] = 1;
	  users[source_i]++;
	  update_stack(target_i, target_slot, source_i, IMMUTABLE, TYPE_CONST_PTR);
	} else if(action == 6) { // mut_ref deref_mut
	  int source_slot = nondet_target();
	  int target_slot = nondet_target();
	  __CPROVER_assume(source_slot >= 0 && source_slot < N_REF_MUT);
	  __CPROVER_assume(target_slot >= 0 && target_slot < N_MUT_PTR);
	  int source_i = i_of_src(source_slot, TYPE_REF_MUT);
	  int target_i = i_of_src(target_slot, TYPE_MUT_PTR);
	  __CPROVER_assume(live[source_i] == 1);
	  __CPROVER_assume(live[target_i] == 0);
	  __CPROVER_assume(users[source_i] == 0);
	  t_ptr[target_slot] = ref_mut_deref_mut(&mut_refs[source_slot]);
	  live[target_i] = 1;
	  users[source_i] = -1;
	  update_stack(target_i, target_slot, source_i, MUTABLE, TYPE_MUT_PTR);
	}
	for(int j = 0; j < N_MUT_PTR; j++) {
	  for(int k = 0; k < N_MUT_PTR; k++) {
		assert(live[i_of_src(j, TYPE_MUT_PTR)] == 0 || live[i_of_src(k, TYPE_CONST_PTR)] || t_ptr[i_of_src(j, TYPE_MUT_PTR)] != const_t_ptr[i_of_src(k, TYPE_CONST_PTR)]);
		if(j == k) { continue; }
		assert(live[i_of_src(j, TYPE_MUT_PTR)] == 0 || live[i_of_src(k, TYPE_MUT_PTR)] || t_ptr[i_of_src(j, TYPE_MUT_PTR)] != t_ptr[i_of_src(k, TYPE_MUT_PTR)]);
	  }
	}
  }
  return 0;
}

/*
 * infer mut borrow from lifetimes and whether the ARUGMENT reference type is mutable
 * raise an error if lifetime arguments appear in some other context than rerference types
 */
