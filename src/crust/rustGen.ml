let max_mut_seq = 4;;
let max_immut_seq = 2;;

module MTSet = TypeUtil.MTSet

type seq_state = {
  public_types : MTSet.t;
  t_list : Types.mono_type list
}

let memo_get_fn_types = 
  let cache = Hashtbl.create 50 in
  fun fn_inst ->
    if Hashtbl.mem cache fn_inst then
      Hashtbl.find cache fn_inst
    else
      let fn_def = Env.EnvMap.find Env.fn_env @@ fst fn_inst in
      let t_bindings = Types.type_binding fn_def.Ir.fn_tparams @@ snd fn_inst in
      let arg_types = List.map (fun (_,t) -> TypeUtil.to_monomorph t_bindings t) fn_def.Ir.fn_args in
      let ret_type = TypeUtil.to_monomorph t_bindings fn_def.Ir.ret_type in
      Hashtbl.add cache fn_inst (arg_types,ret_type);
      (arg_types,ret_type)

let get_init_types () = 
  match snd @@ memo_get_fn_types ("crust_init",[]) with
  | `Tuple tl -> tl
  | _ -> assert false

let drop_fn = "__DROP_ACTION__";;

let counter = ref 0;;

let fresh_name () = 
  let to_ret = "crust_test_" ^ string_of_int (!counter) in
  incr counter;
  to_ret

type method_arg = 
  | Primitive of string
  | Var of string
  | Var_ref of string
  | Var_mut_ref of string
  | Tuple of method_arg list


module MTMap = Map.Make(struct
    type t = Types.mono_type
    let compare = Pervasives.compare
end)

module SMap = Map.Make(String)

type ty_state = {
  var_of_ty : SSet.t MTMap.t;
  var_stack : string list;
  ty_of_var : Types.mono_type SMap.t
}

type action_call = 
  | Open_Block of string * string * (method_arg list)
  | Close_Block

class rust_pp buf = object(self)
  inherit Emit.emitter buf
  val mutable var_counter = 0
    
  method emit_sequence sequence =
    var_counter <- 0;
    let init_vars,concrete_call_seqs = self#concretize sequence in
    List.iter (fun cc_seq ->
        self#emit_cc_sequence init_vars cc_seq
    ) concrete_call_seqs

  method private init_state = 
    let init_vars = List.map (fun ty ->
        (ty,self#next_var)
      ) @@ get_init_types ()  in
    let var_of_ty = List.fold_left (fun accum (ty,var) ->
        if MTMap.mem ty accum then
          MTMap.add ty (SSet.add var (MTMap.find ty accum)) accum
        else
          MTMap.add ty (SSet.singleton var) accum
      ) MTMap.empty init_vars in
    let ty_of_var = List.fold_left (fun accum (ty,var) ->
        SMap.add var ty accum
      ) SMap.empty init_vars in
    List.map snd init_vars,{
      var_of_ty = var_of_ty;
      var_stack = [];
      ty_of_var = ty_of_var
    }

  method private concretize call_seq = 
    var_counter <- 0;
    let init_vars,init_state = self#init_state in
    init_vars,(self#concretize_aux init_state [] [] call_seq)
           
  method private nondet_fn ty = 
    match ty with
    | `Int w -> "crust_nondet_i" ^ (string_of_int w) ^ "()"
    | `Unit -> "()"
    | `Bool -> "crust_nondet_bool()"
    | `Float w -> "crust_nondet_f" ^ (string_of_int w) ^ "()"
    | `UInt w -> "crust_nondet_u" ^ (string_of_int w) ^ "()"
    | `Char -> "crust_nondet_char()"

  method private update_drop state = 
    match state.var_stack with
    | [] -> assert false
    | h::t -> 
      let var_t = SMap.find h state.ty_of_var in
      let v_set = MTMap.find var_t state.var_of_ty in
      {
        var_stack = t;
        var_of_ty = MTMap.add var_t (SSet.remove h v_set) state.var_of_ty;
        ty_of_var = SMap.remove h state.ty_of_var
      }

  method private update_call state ret_type out_var = 
    let var_set = 
      if not (MTMap.mem ret_type state.var_of_ty) then
        SSet.empty
      else
        MTMap.find ret_type state.var_of_ty
    in
    {
      var_stack = out_var::state.var_stack;
      var_of_ty = MTMap.add ret_type (SSet.add out_var var_set) state.var_of_ty;
      ty_of_var = SMap.add out_var ret_type state.ty_of_var
    }

  method private concretize_aux state accum cc call_seq = 
    match call_seq with
    | [] -> (List.rev cc)::accum
    | (df,[])::t when df = drop_fn ->
      self#concretize_aux (self#update_drop state) accum (Close_Block::cc) t
    | ((fn,m_args) as inst)::t ->
      let (arg_types,ret_type) = memo_get_fn_types inst in
      let (call_combinations : method_arg list list) = self#get_args state arg_types in
      let out_var = self#next_var in
      List.fold_left (fun accum c_args ->
          self#concretize_aux (self#update_call state ret_type out_var) accum ((Open_Block (out_var,fn,c_args))::cc) t
        ) accum call_combinations

  method private get_args state arg_types = 
    self#get_args_aux [] [] state arg_types

  method private get_args_aux (t_accum : method_arg list list) (a_accum : method_arg list) state arg_types =
    match arg_types with
    | [] -> (List.rev a_accum)::t_accum
    | h::t -> 
      List.fold_left (fun t_accum e ->
          self#get_args_aux t_accum (e::a_accum) state t
        ) t_accum (self#resolve_argument state h)

  method private resolve_argument = 
    let build_var_choice state t = 
      let vars = MTMap.find t state.var_of_ty in
      SSet.fold (fun v accum ->
          (Var v)::accum
        ) vars [] in
    fun state arg_type ->
      match arg_type with
      | #Types.simple_type as s -> [Primitive (self#nondet_fn s)]
      | `Ptr_Mut _
      | `Ptr _ -> failwith "Unsupported pointer type in public api"
      | `Bottom -> failwith "Unsupported bottom type"
      | `Str
      | `Vec _ -> failwith "stray dst type"
      | (`Ref (_,t') as t) ->
        if MTMap.mem t state.var_of_ty then
          build_var_choice state t
        else
          let vars = MTMap.find t' state.var_of_ty in
          SSet.fold (fun v accum ->
              (Var_ref v)::accum
            ) vars []
      | (`Ref_Mut (_,t') as t) ->
        if MTMap.mem t state.var_of_ty then
          build_var_choice state t
        else
          let vars = MTMap.find t' state.var_of_ty in
          SSet.fold (fun v accum ->
              (Var_mut_ref v)::accum
            ) vars []
      | (`Tuple tl as t) ->
        if MTMap.mem t state.var_of_ty then
          build_var_choice state t
        else
          let arg_insts = self#get_args state tl in
          List.map (fun l ->
              Tuple l
            ) arg_insts
      | `Adt_type _
      | `Fixed_Vec _ ->
        build_var_choice state arg_type

  method private next_var = 
    let to_ret = "v" ^ (string_of_int var_counter) in
    var_counter <- var_counter + 1;
    to_ret

  method private emit_cc_sequence init_vars cc_sequence = 
    let test_name = fresh_name () in
    self#put_all [ "fn "; test_name; "() "];
    self#open_block ();
    match init_vars with
    | [] -> ()
    | [v] -> self#put_all [ "let ("; v; ",) = crust_init();" ]
    | t -> self#put_all [ "let ("; (String.concat ", " init_vars); ") = crust_init();" ]
      ;
    self#newline ();
    let open_blocks = List.fold_left (fun accum b ->
        match b with
        | Close_Block -> 
          (self#close_block(); self#newline (); (pred accum))
        | Open_Block (v,fn,args) -> begin
            self#open_block ();
            self#put_all [ "let "; v ; " = "; fn; "(" ];
            self#put_many ", " self#emit_arg args;
            self#put_all [ ");" ];
            self#newline ();
            (succ accum)
          end
      ) 0 cc_sequence in
    for i = 0 to (pred open_blocks) do
      self#newline ();
      self#close_block ()
    done;
    self#newline ();
    self#close_block ();
    self#newline ()


  method private emit_arg arg = 
    match arg with
    | Primitive s -> self#put s
    | Var v -> self#put v
    | Var_ref v -> self#put_all [ "&"; v ];
    | Var_mut_ref v -> self#put_all [ "&mut "; v ]
    | Tuple tl -> begin
        self#put "(";
        self#put_many ", " self#emit_arg tl;
        self#put ")"
      end
end

(* (not a) stub! *)
let gen_call = 
  let buf = Buffer.create 1000 in
  let pp = new rust_pp buf in
  fun out_channel sequence ->
  let call_seq = List.rev sequence in
  Printf.fprintf out_channel "//%s\n" (String.concat " -> " @@ List.map fst call_seq);
  Buffer.clear buf;
  pp#emit_sequence @@ List.rev sequence;
  Buffer.output_buffer out_channel buf

  

(* this is a pretty rough heuristic.
 * TODO(jtoman): BMC based mutation checking
 *)
let mut_analysis (fn_name,m_args) = 
  let fn_def = Env.EnvMap.find Env.fn_env fn_name in
  let t_binding = Types.type_binding fn_def.Ir.fn_tparams m_args in
  List.exists (fun (_,t) ->
      match TypeUtil.to_monomorph t_binding t with
      | `Ref_Mut _ -> true
      | `Ptr_Mut _ -> true
      | _ -> false
    ) fn_def.Ir.fn_args

let valid_extend state ((fn_name,m_args) as fn_inst) = 
  if fn_name = drop_fn then
    match state.t_list with
    | [] -> None
    | h::t -> 
      if List.mem h t then
        Some { state with t_list = t }
      else
        Some { t_list = t; public_types = MTSet.remove h state.public_types }
  else
    let (arg_types,ret_type) = memo_get_fn_types fn_inst in
    match TypeUtil.get_inst state.public_types [] (arg_types : Types.mono_type list :> Types.r_type list) with
    | `Mismatch -> prerr_endline "FUCK"; None
    | `Inst _ -> Some { t_list = ret_type::state.t_list; public_types = MTSet.add ret_type state.public_types }

(* this needs to allow move functions in the mut call sequence *)
let rec do_gen_mut mut_fn_set const_fn_set seq_len state index path f = 
(*  prerr_endline @@ "Exploring: " ^ (string_of_int index) ^ " with length -> " ^ (string_of_int seq_len) ^ " more debug -> " ^ (string_of_int @@ List.length path);*)
  if seq_len = max_mut_seq then
    do_gen_immut const_fn_set 0 state 0 path f
  else if index = (Array.length mut_fn_set) then
    ()
  else begin
    (* use this prefix *)
    do_gen_immut const_fn_set 0 state 0 path f;
    (match valid_extend state mut_fn_set.(index) with
      | None -> ()
      | Some st -> do_gen_mut mut_fn_set const_fn_set (succ seq_len) st 0 (mut_fn_set.(index)::path) f
    );
    do_gen_mut mut_fn_set const_fn_set seq_len state (succ index) path f
  end
and do_gen_immut const_fn_set seq_len state index path f =
  if seq_len = max_immut_seq then
    f path
  else if index = (Array.length const_fn_set) then
    f path
  else begin
    match valid_extend state const_fn_set.(index) with
    | Some st -> do_gen_immut const_fn_set (succ seq_len) st (succ index) (const_fn_set.(index)::path) f
    | None -> ()
  end

let gen_call_seq out_channel fi_set = 
  let (mut_fn_set,const_fn_set) = Analysis.FISet.partition mut_analysis fi_set in
  let mut_fn_arr = Array.of_list @@ (drop_fn,[])::Analysis.FISet.elements mut_fn_set in
  let const_fn_arr = Array.of_list @@ Analysis.FISet.elements const_fn_set in
  do_gen_mut mut_fn_arr const_fn_arr 0 {
    public_types = List.fold_left (fun accum t -> MTSet.add t accum) MTSet.empty @@ get_init_types ();
    t_list = []
  } 0 [] (gen_call out_channel)

let gen_driver out_channel pt_set pf_set = 
  gen_call_seq out_channel pf_set
