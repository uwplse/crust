type adt_env_t = (string,Ir.type_expr) Hashtbl.t
let (adt_env : adt_env_t) = Hashtbl.create 10;;
let (fn_env : (string,Ir.fn_def) Hashtbl.t) = Hashtbl.create 10;;
