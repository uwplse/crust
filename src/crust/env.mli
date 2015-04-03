module EnvMap : sig
  type 'b t
  val find : 'b t -> string -> 'b
  val fold : (string -> 'b -> 'c -> 'c) -> 'b t -> 'c -> 'c
  val mem : 'b t -> string -> bool
end

exception Missing_binding of string
type adt_env_t = Ir.type_expr EnvMap.t
val adt_env : adt_env_t
val fn_env : Ir.fn_def EnvMap.t
val abstract_fn_env : Ir.abstract_fn_def EnvMap.t
val static_env : (Types.r_type * Ir.expr) EnvMap.t
val abstract_impl : string list EnvMap.t
val init_opt : bool ref
val driver_env : Ir.expr list ref

val set_env : Ir.module_expr list -> unit
(* convenience function *)
val get_adt_drop : string -> string option
val is_abstract_fn : string -> bool
val is_static_var : string -> bool

val associated_types : Types.associated_type list EnvMap.t

val crust_init_name : unit -> string option
val crust_init_name_e : unit -> string
