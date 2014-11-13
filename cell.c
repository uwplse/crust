#include<stdlib.h>
#include<assert.h>
#define option_t(type) option_##type##_t
#define option_t_impl(type) typedef struct { option_flag_t flag; type maybe; } option_t(type);
//#define __CPROVER_assume(x)

struct T {
  int a;
};

typedef enum {
  SOME,
  NONE
} option_flag_t;

typedef struct unsafe_cell {
  struct T value;
} unsafe_cell_t;

typedef struct ref_cell {
  int borrow;
  struct unsafe_cell value;
} ref_cell_t;


typedef struct {
  ref_cell_t *parent;
} ref_mut_t;

option_t_impl(ref_mut_t)

typedef struct {
  ref_cell_t *parent;
} ref_t;

option_t_impl(ref_t)

unsafe_cell_t unsafe_cell_new(struct T value) {
  unsafe_cell_t to_ret = { value };
  return to_ret;
}

struct T *unsafe_cell_get(unsafe_cell_t *self) {
  return &(self->value);
}

struct T unsafe_cell_unwrap(unsafe_cell_t *self) {
  return self->value;
}

ref_cell_t ref_cell_new(struct T value) {
  ref_cell_t to_ret = { 0, unsafe_cell_new(value) };
  return to_ret;
}

struct T ref_cell_unwrap(ref_cell_t *self) {
  return unsafe_cell_unwrap(&self->value);
}

option_t(ref_t) ref_cell_try_borrow(ref_cell_t *self) {
  if(self->borrow == -1) {
	option_t(ref_t) to_ret;
	to_ret.flag = NONE;
	return to_ret;
  } else {
	option_t(ref_t) to_ret;
	self->borrow = self->borrow + 1;
	to_ret.maybe.parent = self;
	return to_ret;
  }
}

option_t(ref_mut_t) ref_cell_try_mut_borrow(ref_cell_t *self) {
  if(self->borrow == 0) {
	option_t(ref_mut_t) to_ret;
	to_ret.flag = SOME;
	to_ret.maybe.parent = self;
	self->borrow = -1;
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
  self->parent->borrow = self->parent->borrow - 1;
}

const struct T *ref_deref(ref_t *self) {
  return unsafe_cell_get(&self->parent->value);
}

void ref_mut_drop(ref_mut_t *self) {
  self->parent->borrow = self->parent->borrow - 1;
}

struct T *ref_mut_deref_mut(ref_mut_t *self) {
  return unsafe_cell_get(&self->parent->value);
}

const struct T *ref_mut_deref(ref_mut_t *self) {
  return unsafe_cell_get(&self->parent->value);
}

short nondet_action();
short nondet_target();

int main(int argc, char **argv) {
  struct T val = { 2 };
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
		assert(m_flags[j] == 0 || m_flags[k] == 0 || mut_refs[j].parent != mut_refs[k].parent);
		assert(m_flags[j] == 0 || im_flags[k] == 0 || mut_refs[j].parent != refs[k].parent);
	  }
	}
  }
  return 0;
}
