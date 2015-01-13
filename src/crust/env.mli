module EnvMap : sig
  type 'b t
  val find : 'b t -> string -> 'b
  val fold : (string -> 'b -> 'c -> 'c) -> 'b t -> 'c -> 'c
end
exception Missing_binding of string
type adt_env_t = Ir.type_expr EnvMap.t
val adt_env : adt_env_t
val fn_env : Ir.fn_def EnvMap.t
val gcc_mode : bool ref
val set_env : Ir.module_expr list -> unit
