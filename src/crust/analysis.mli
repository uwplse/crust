module FISet : Set.S with type elt = string * (Types.mono_type list)
module TISet : Set.S with type elt = TypeUtil.type_inst


type borrow_nested = [
  | `Tuple of int  * borrow_nested
  | `MutableBorrow of int
  | `ImmutableBorrow of int
]

type borrow_info = [
  | borrow_nested
  | `NoBorrow
]

type move_info = [
  | `Move_val of int
  | `Move_tuple of int * (move_info list)
]

type walk_state = {
  type_inst : TISet.t;
  fn_inst : FISet.t;
  public_type : TypeUtil.MTSet.t;
  public_fn : FISet.t;
  static_var : SSet.t
}

exception ResolutionFailed of string

val run_analysis : unit -> walk_state
val borrow_analysis : Ir.fn_def -> borrow_info
val resolve_abstract_fn : string -> Types.mono_type list -> (Types.mono_type list * string)
val init_fn_filter : string -> unit
val set_fn_filter : string -> unit
val move_analysis : Types.mono_type list -> move_info list
