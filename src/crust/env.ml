type adt_env_t = (string,Ir.type_expr) Hashtbl.t
let (adt_env : adt_env_t) = Hashtbl.create 10;;
let (fn_env : (string,Ir.fn_def) Hashtbl.t) = Hashtbl.create 10;;

let rec set_env = function 
  | [] -> ()
  | ((`Enum_def {
		Ir.enum_name = adt_name;
		_
	}) as adt)::t
  | ((`Struct_def {
		Ir.struct_name = adt_name;
		_
	  }) as adt)::t -> 
	 Hashtbl.add adt_env adt_name adt;
	 set_env t
  | (`Fn f)::t -> 
	 Hashtbl.add fn_env f.Ir.fn_name f;
	 set_env t

let gcc_mode = ref false;;
