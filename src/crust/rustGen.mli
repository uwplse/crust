val gen_driver : string -> int -> unit 
val dump_api : out_channel -> TypeUtil.MTSet.t -> Analysis.FISet.t -> unit

val no_mut_analysis : bool ref
val skip_interesting_check : bool ref
val skip_symm_break : bool ref
val skip_interfere_check : bool ref
val skip_copy_use : bool ref

val mut_action_len : int ref
val immut_action_len : int ref
val assume_ident_init : bool ref
val infer_api_only : bool ref
