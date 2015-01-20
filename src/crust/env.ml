exception Missing_binding of string
module EnvMap = struct
  type 'b t = (string, 'b) Hashtbl.t
  let find h k = 
    try
      Hashtbl.find h k
    with Not_found ->
      raise (Missing_binding ("No binding found for object: " ^ k))
  let fold = Hashtbl.fold
  let mem = Hashtbl.mem
end

module EnvSet = struct
  type 'b t = ('b,unit) Hashtbl.t
  let mem = Hashtbl.mem
end

type adt_env_t = (string,Ir.type_expr) Hashtbl.t
let (adt_env : adt_env_t) = Hashtbl.create 10;;
let (fn_env : (string,Ir.fn_def) Hashtbl.t) = Hashtbl.create 10;;
let type_infr_filter = Hashtbl.create 10;;

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
let init_opt = ref false;;

let init_inference_filter file_name = 
  let f_in = open_in file_name in
  let rec read_loop () = 
    try
      let l = input_line f_in in
      if l = "" then read_loop () else (
        Hashtbl.add type_infr_filter l ();
        read_loop ()
      )
    with End_of_file -> close_in f_in
  in
  read_loop ()
