module FISet : Set.S with type elt = string * (Types.mono_type list)
module TISet : Set.S with type elt = string * (Types.mono_type list)
module MTSet : Set.S with type elt = Types.mono_type

val do_test : Types.mono_type list -> Types.r_type list -> [
  | `Mismatch
  | `Inst of (string * Types.mono_type) list list
  ]
val debug_get_constructors : unit -> string list

type walk_state = {
	type_inst : TISet.t;
	fn_inst : FISet.t;
	public_type : MTSet.t;
	public_fn : FISet.t
  }

val run_analysis : unit -> walk_state
