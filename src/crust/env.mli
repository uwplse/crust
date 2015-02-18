module EnvMap : sig
  type 'b t
  val find : 'b t -> string -> 'b
  val fold : (string -> 'b -> 'c -> 'c) -> 'b t -> 'c -> 'c
  val mem : 'b t -> string -> bool
end
module EnvSet : sig
  type 'b t
  val mem : 'b t -> 'b -> bool
end
exception Missing_binding of string
type adt_env_t = Ir.type_expr EnvMap.t
val adt_env : adt_env_t
val fn_env : Ir.fn_def EnvMap.t
val static_env : (Types.r_type * Ir.expr) EnvMap.t
val abstract_impl : string list EnvMap.t
val gcc_mode : bool ref
val init_opt : bool ref
val type_infr_filter : string EnvSet.t

val set_env : Ir.module_expr list -> unit
val init_inference_filter : string -> unit
(* convenience function *)
val get_adt_drop : string -> string option
val is_abstract_fn : string -> bool
val is_static_var : string -> bool

val associated_types : Types.associated_type list EnvMap.t
