module FISet : Set.S with type elt = string * (Types.mono_type list)
module TISet : Set.S with type elt = TypeUtil.type_inst

type walk_state = {
  type_inst : TISet.t;
  fn_inst : FISet.t;
  public_type : TypeUtil.MTSet.t;
  public_fn : FISet.t;
  static_var : SSet.t
}

exception ResolutionFailed of string

val run_analysis : unit -> walk_state
val run_test_analysis : unit -> walk_state
val resolve_abstract_fn : string -> Types.mono_type list -> Types.mono_type list -> (Types.mono_type list * string)
val init_fn_filter : string -> unit
val set_fn_filter : string -> unit
val init_type_filter : string -> unit
