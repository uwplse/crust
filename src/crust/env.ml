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
let adt_env = Hashtbl.create 10;;
let fn_env = Hashtbl.create 10;;
let type_infr_filter = Hashtbl.create 10;;
let abstract_impl = Hashtbl.create 10;;
let associated_types = Hashtbl.create 10;;

let is_abstract name = Hashtbl.mem abstract_impl name

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
    (match f.Ir.fn_impl with
     | None -> ()
     | Some { Ir.abstract_name = a_name } ->
       if Hashtbl.mem abstract_impl a_name then
         Hashtbl.replace abstract_impl a_name @@ f.Ir.fn_name :: (Hashtbl.find abstract_impl a_name)
       else
         Hashtbl.add abstract_impl a_name [ f.Ir.fn_name ]
    );
    set_env t
  | `Assoc_type ({ Types.abstract_name = a_name; _ } as a)::t ->
    (if Hashtbl.mem associated_types a_name then 
      Hashtbl.replace associated_types a_name @@ a::(Hashtbl.find associated_types a_name)
    else
      Hashtbl.add associated_types a_name [ a ]
    );
    set_env t
  | `Abstract_Fn _::t ->
    set_env t


let gcc_mode = ref false;;
let init_opt = ref false;;

let is_abstract_fn = Hashtbl.mem abstract_impl

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

let get_adt_drop t_name = 
  let t_def = Hashtbl.find adt_env t_name in
  match t_def with
  | `Enum_def e -> e.Ir.drop_fn
  | `Struct_def e -> e.Ir.drop_fn
