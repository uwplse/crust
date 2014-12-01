module FISet : Set.S with type elt = string * (Types.mono_type list)
module TISet : Set.S with type elt = string * (Types.mono_type list)
module MTSet : Set.S with type elt = Types.mono_type
(*
type borrow_nested = [
  | `Tuple of int * borrow_nested
  | `MutableBorrow of int
  | `ImmutableBorrow of int
]

type borrow_info = [
  | borrow_nested
  | `NoBorrow
]
*)

type borrow_info = 
  | MutableBorrow of int
  | ImmutableBorrow of int
  | NoBorrow

type walk_state = {
	type_inst : TISet.t;
	fn_inst : FISet.t;
	public_type : MTSet.t;
	public_fn : FISet.t
  }

val run_analysis : unit -> walk_state
val borrow_analysis : Ir.fn_def -> borrow_info
