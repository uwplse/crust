type adt_env_t = (string,Ir.type_expr) Hashtbl.t
val adt_env : adt_env_t
val fn_env : (string,Ir.fn_def) Hashtbl.t


