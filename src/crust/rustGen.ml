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
  | Open_Block of string * Types.mono_type * string * (method_arg list)
  | Close_Block

module UnionFind = struct
  (* a very stupid union find implementation  
   * that doesn't use the height optimization
  *)

  type 'a t = ('a,'a) Hashtbl.t

  let add_element uf a = Hashtbl.add uf a a

  let ensure_add uf a = 
    if Hashtbl.mem uf a then () else add_element uf a

  let find uf a =
    let rec find_loop a = 
      let r = Hashtbl.find uf a in
      if r = a then r else find_loop r
    in
    find_loop a

  let union uf a b =
    ensure_add uf a;
    ensure_add uf b;
    let a_rep = find uf a in
    let b_rep = find uf b in
    if a_rep = b_rep then
      ()
    else
      Hashtbl.replace uf b_rep a_rep

  let create () = Hashtbl.create 10
  
  (*let all_congruence uf = 
    let representatives = Hashtbl.fold (fun k v accum ->
        if k = v then
          k::accum
        else accum
      ) uf [] in
    List.length representatives = 1*)
end

class rust_pp buf = object(self)
  inherit Emit.emitter buf
  val mutable var_counter = 0
    
  method emit_sequence sequence =
    var_counter <- 0;
    let init_vars,concrete_call_seqs = self#concretize sequence in
    let interfering_calls = List.filter self#compute_interference concrete_call_seqs in
    let ic_nosym = self#break_symmetry interfering_calls in
    List.iter (fun cc_seq ->
        self#emit_cc_sequence init_vars cc_seq
    ) ic_nosym

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
      let call_combinations = self#get_args state arg_types in
      let out_var = self#next_var in
      List.fold_left (fun accum c_args ->
          self#concretize_aux (self#update_call state ret_type out_var) accum ((Open_Block (out_var,ret_type,fn,c_args))::cc) t
        ) accum call_combinations

  method private get_args state arg_types = 
    self#get_args_aux [] [] state arg_types

  method private get_args_aux t_accum a_accum state arg_types =
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

  val replace_regex = Str.regexp_string "$"

  method private rust_name fn_name =
    let fn_def = Env.EnvMap.find Env.fn_env fn_name in
    let to_transmute =
      match fn_def.Ir.fn_impl with
      | None -> fn_name
      | Some i -> i.Ir.abstract_name
    in
    Str.global_replace replace_regex "::" to_transmute

  method private emit_cc_sequence init_vars cc_sequence = 
    let test_name = fresh_name () in
    self#put_all [ "fn "; test_name; "() "];
    self#open_block ();
    (match init_vars with
    | [] -> ()
    | [v] -> self#put_all [ "let (mut "; v; ",) = crust_init();" ]
    | t -> self#put_all [ "let ("; (String.concat ", " @@ List.map (fun v -> "mut " ^ v) init_vars); ") = crust_init();" ]
    );
    self#newline ();
    let open_blocks = List.fold_left (fun accum b ->
        match b with
        | Close_Block -> 
          (self#close_block(); self#newline (); (pred accum))
        | Open_Block (v,_,fn,args) -> begin
            let rust_name = self#rust_name fn in
            self#open_block ();
            self#put_all [ "let mut "; v ; " = "; rust_name; "(" ];
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

  method private compute_interference cc_seq = 
    let ref_vars = self#walk_call_seq SSet.add SSet.empty cc_seq in
    (* now compute the congruence closure *)
    let uf = UnionFind.create () in
    SSet.iter (fun v ->
        UnionFind.add_element uf v
      ) ref_vars;
    List.iter (function 
        | Close_Block -> ()
        | Open_Block (v,_,_,l) ->
          let ref_vars = self#walk_args SSet.add SSet.empty l in
          let to_union = v in
          SSet.iter (fun elem ->
              UnionFind.union uf elem to_union
            ) ref_vars
      ) cc_seq;
    (* now ensure that all referenced variables are in the same congruence class *)
    let rep_vars = SSet.fold (fun elem accum ->
        SSet.add (UnionFind.find uf elem) accum
      ) ref_vars SSet.empty in
    SSet.cardinal rep_vars = 1

  method private walk_call_seq :
    'a.(string -> 'a -> 'a) -> 'a -> action_call list -> 'a = fun f accum cc_seq ->
    match cc_seq with
    | Close_Block::t -> self#walk_call_seq f accum t
    | Open_Block (_,_,_,l)::t -> self#walk_call_seq f (self#walk_args f accum l) t
    | [] -> accum

  method private walk_args :
    'a.(string -> 'a -> 'a) -> 'a -> method_arg list -> 'a =
    fun f accum m_list ->
    List.fold_left (fun accum l ->
        match l with
        | Primitive _ -> accum
        | Var v
        | Var_ref v 
        | Var_mut_ref v -> f v accum
        | Tuple tl -> self#walk_args f accum tl
      ) accum m_list

  method private is_symmetric vc1 vc2 =
    let rec is_symmetric_loop sym_map s1 s2 =
      match s1,s2 with
      | (h1::t1),(h2::t2) -> 
        if SMap.mem h1 sym_map then
          if (SMap.find h1 sym_map) = h2 then
            is_symmetric_loop sym_map t1 t2
          else
            false
        else
          is_symmetric_loop (SMap.add h1 h2 sym_map) t1 t2
      | [],[] -> true
      | _,_ -> assert false
    in
    is_symmetric_loop SMap.empty vc1 vc2

  method private break_symmetry cc_seqs =
    let cc_vc = List.map (fun cc ->
        cc,List.rev @@ self#walk_call_seq (fun v l -> v::l) [] cc
      ) cc_seqs in
    let rec break_loop l = match l with
      | [] -> []
      | (cc,var_choice)::t ->
        let t' = List.filter (fun (_,var_choice') ->
            not @@ self#is_symmetric var_choice var_choice'
          ) t in
        cc::(break_loop t')
    in
    break_loop cc_vc
end

let rec has_drop ty = match ty with
  | #Types.simple_type -> false
  | `Tuple tl -> List.exists has_drop tl
  | `Vec _ -> false
  | `Str -> false
  | `Fixed_Vec (_,t) -> has_drop t
  | `Bottom 
  | `Ref _
  | `Ref_Mut _ 
  | `Ptr _
  | `Ptr_Mut _ -> false
  | `Adt_type a -> match (Env.get_adt_drop a.Types.type_name) with
    | Some _ -> true
    | None -> false
                  
  

let filter_drop call_seq = 
  let (dropped_types,_) =
    List.fold_left (fun (dropped_types,type_stack) ((fn_name,args) as inst) -> 
      if fn_name = drop_fn then
        match type_stack with
        | h::t -> (h::dropped_types,t)
        | _ -> assert false
      else
        let (_,ret_type) = memo_get_fn_types inst in
        (dropped_types,ret_type::type_stack)
    ) ([],[]) call_seq in
  match dropped_types with 
  | [] -> true
  | _ -> 
    (*let a = *)
    List.exists has_drop dropped_types
    (*in
    Printf.fprintf stdout "// RESULT -> %b\n" a;
    if a then begin
      Printf.fprintf stdout "// drop judgment: ";
      List.iter (fun ty ->
          Printf.fprintf stdout "%s -> %b, " (Types.pp_t (ty :  Types.mono_type :> Types.r_type)) @@ has_drop ty
        ) dropped_types
    end else ();
    a*)

(* (not a) stub! *)
let gen_call = 
  let buf = Buffer.create 1000 in
  let pp = new rust_pp buf in
  fun out_channel sequence ->
  let call_seq = List.rev sequence in
  if not @@ filter_drop call_seq then () else begin
    Printf.fprintf out_channel "//%s\n" (String.concat " -> " @@ List.map fst call_seq);
    Buffer.clear buf;
    pp#emit_sequence @@ List.rev sequence;
    Buffer.output_buffer out_channel buf
  end

  

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
    | `Mismatch -> None
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

let prelude = [
  "#![crate_type = \"lib\"]";
  "#![no_std]";
  "#![feature(unsafe_destructor)]";
  "extern crate core;";
  "extern crate alloc;"
]

let output_standard_header out_channel = 
  List.iter (Printf.fprintf out_channel "%s\n") prelude

let gen_call_seq out_channel fi_set = 
  let (mut_fn_set,const_fn_set) = Analysis.FISet.partition mut_analysis fi_set in
  let mut_fn_arr = Array.of_list @@ (drop_fn,[])::Analysis.FISet.elements mut_fn_set in
  let const_fn_arr = Array.of_list @@ Analysis.FISet.elements const_fn_set in
  do_gen_mut mut_fn_arr const_fn_arr 0 {
    public_types = List.fold_left (fun accum t -> MTSet.add t accum) MTSet.empty @@ get_init_types ();
    t_list = []
  } 0 [] (gen_call out_channel)

let gen_driver out_channel pt_set pf_set = 
  output_standard_header out_channel;
  gen_call_seq out_channel pf_set
