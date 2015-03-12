val gen_driver : string -> int -> TypeUtil.MTSet.t -> Analysis.FISet.t -> unit 
val dump_api : out_channel -> TypeUtil.MTSet.t -> Analysis.FISet.t -> unit

val no_mut_analysis : bool ref
val mut_action_len : int ref
val immut_action_len : int ref
val assume_ident_init : bool ref
val infer_api_only : bool ref
