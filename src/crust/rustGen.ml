let mut_action_len = ref 4;;
let immut_action_len = ref 2;;
let assume_ident_init = ref true;;
let no_mut_analysis = ref false;;

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

let init_types () = 
  match snd @@ memo_get_fn_types (Env.crust_init_name_e (),[]) with
  | `Tuple tl -> tl
  | _ -> assert false

type api_actions = 
  | Copy
  | Drop
  | Fn of string * (Types.mono_type list)

let counter = ref 0;;

let fresh_name () = 
  let to_ret = "crust_test_" ^ string_of_int (!counter) in
  incr counter;
  to_ret

let unmangle_name = 
  let replace_regex = Str.regexp_string "$" in
  Str.global_replace replace_regex "::"

let rust_name fn_name =
  let fn_def = Env.EnvMap.find Env.fn_env fn_name in
  let to_transmute =
    match fn_def.Ir.fn_impl with
    | None -> fn_name
    | Some i -> i.Ir.abstract_name
  in
  unmangle_name to_transmute
  


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
  | Ptr_Assert of [`Immut | `Mut ] * string * string
  | Null_Assert of string
  | Close_Block
  | Copy_Action of string * string

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
end

let prelude = [
  "#![crate_type = \"lib\"]";
  "#![no_std]";
  "#![feature(unsafe_destructor)]";
  "extern crate core;";
  "extern crate alloc;"
]

class rust_pp buf output_file_prefix num_tests = object(self)
  inherit Emit.emitter buf
  val mutable var_counter = 0
  val mutable curr_tests = 0
  val mutable test_counter = 0

  val mutable init_arg_list = []

  method emit_sequence sequence =
    var_counter <- 0;
    self#debug_call_seq sequence;
    let init_vars,concrete_call_seqs = self#concretize sequence in
    let final_call_seq = 
      List.filter self#all_copy_var_used @@
      if !assume_ident_init then
        self#break_symmetry @@ List.filter self#compute_interference concrete_call_seqs
      else
        concrete_call_seqs
    in
    List.iter (fun cc_seq ->
        self#emit_cc_sequence init_vars cc_seq
    ) final_call_seq

  method private debug_call_seq action_seq = 
    self#put_i "// " ;
    self#put_many " -> " (function 
        | Drop -> self#put "<DROP>"
        | Copy -> self#put "<COPY>"
        | Fn (f_name,_) -> self#put f_name
      ) action_seq;
    self#newline ()
      

  method private reset_buffer () =
    Buffer.clear buf

  method flush_buffer () = 
    if curr_tests = 0 then () else begin
      let file_name = Printf.sprintf "%s_%d.rs" output_file_prefix test_counter in
      let f = open_out file_name in
      List.iter (fun l ->
          output_string f l;
          output_string f "\n"
        ) prelude;
      Buffer.output_buffer f buf;
      close_out f;
      self#reset_buffer ();
      curr_tests <- 0;
      test_counter <- test_counter + 1
    end

  method private ensure_limit () = 
    if curr_tests = num_tests then
      self#flush_buffer ()
    else ()
    

  method private init_state = 
    let init_vars = List.map (fun ty ->
        (ty,self#next_var)
      ) @@ init_types ()  in
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
    | `Int w -> "nondet_crust_i" ^ (Types.string_of_intsize w) ^ "()"
    | `Unit -> "()"
    | `Bool -> "nondet_crust_bool()"
    | `Float w -> "nondet_crust_f" ^ (string_of_int w) ^ "()"
    | `UInt w -> "nondet_crust_u" ^ (Types.string_of_intsize w) ^ "()"
    | `Char -> "nondet_crust_char()"

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
    | Drop::t ->
      self#concretize_aux (self#update_drop state) accum (Close_Block::cc) t
    | Copy::t ->
      let out_var = self#next_var in
      let copy_var = List.hd state.var_stack in
      let var_type = SMap.find copy_var state.ty_of_var in
      self#concretize_aux (self#update_call state var_type out_var) accum ((Copy_Action (out_var,copy_var))::cc) t
    | (Fn (fn,m_args))::t ->
      let (arg_types,ret_type) = memo_get_fn_types (fn,m_args) in
      let call_combinations = self#get_args state arg_types in
      let out_var = self#next_var in
      let ref_assertions = self#gen_assertions state out_var ret_type in
      List.fold_left (fun accum c_args ->
          self#concretize_aux (self#update_call state ret_type out_var) accum (ref_assertions@((Open_Block (out_var,ret_type,fn,c_args))::cc)) t
        ) accum call_combinations

  method private gen_ty_assertions state assert_gen ty =
    if MTMap.mem ty state.var_of_ty then
      SSet.fold (fun t_var accum ->
          (assert_gen t_var)::accum
        ) (MTMap.find ty state.var_of_ty) []
    else
      []

  method private gen_null_assert var = 
    Null_Assert var

  method private gen_assertions state out_var returned_type = 
    match returned_type with
    | (`Ref_Mut (_,t) as mut_ref) ->
      (self#gen_ty_assertions state (fun v2 ->
           Ptr_Assert (`Mut,out_var,v2)) mut_ref
      ) @ (self#gen_ty_assertions state (fun v2 ->
          Ptr_Assert (`Immut,out_var,v2)) (`Ref (TypeUtil.dummy_lifetime,t))) 
        @ [ self#gen_null_assert out_var ]
    | `Ref (_,t) ->
      self#gen_ty_assertions state (fun v1 ->
          Ptr_Assert (`Immut,v1,out_var)) (`Ref_Mut (TypeUtil.dummy_lifetime,t))
      @ [ self#gen_null_assert out_var ]
    | _ -> []
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

  method private emit_cc_sequence init_vars cc_sequence = 
    self#ensure_limit ();
    curr_tests <- curr_tests + 1;
    let test_name = fresh_name () in
    self#put_all [ "fn "; test_name; "() "];
    self#open_block ();
    let init_name = rust_name @@ Env.crust_init_name_e () in
    let put_args () = 
      self#put_all [ init_name; "(" ];
      self#put_many ", " self#emit_arg init_arg_list;
      self#put ");"
    in
    (match init_vars with
    | [] -> ()
    | [v] -> self#put_all [ "let (mut "; v; ",) = " ]; put_args ()
    | t -> self#put_all [ "let ("; (String.concat ", " @@ List.map (fun v -> "mut " ^ v) init_vars); ") = " ]; put_args ()
    );
    self#newline ();
    let open_blocks = List.fold_left (fun accum b ->
        match b with
        | Close_Block -> 
          (self#close_block(); self#newline (); (pred accum))
        | Ptr_Assert (mut_flag,v1,v2) -> begin
            self#put_all [ "crust_assert("; v1; " as *mut _ as u64 != "; v2; " as *";
                           (match mut_flag with `Mut -> "mut" | `Immut -> "const");
                           " _ as u64);"];
            self#newline ();
            accum
          end
        | Null_Assert out_var ->
          self#put_all [
             "crust_assert("; out_var; " as *mut _ as u64 != 0);"
          ];
          self#newline ();
          accum
        | Copy_Action (out_var,copy_var) -> 
          self#open_block ();
          self#put_all [ "let mut "; out_var; " = " ; copy_var; ";"];
          self#newline();
          (succ accum)
        | Open_Block (v,_,fn,args) -> begin
            let rust_name = rust_name fn in
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

  method dump_compilation_state state =
    prerr_endline "--> DEBUG";
    prerr_endline @@ "--> VARIABLES: " ^ (String.concat ", " state.var_stack);
    prerr_endline "--> VARIABLE TYPES:";
    SMap.iter (fun k v ->
        prerr_endline @@  "===> " ^ k ^ " -> " ^ (Types.pp_t (v : Types.mono_type :> Types.r_type))
    ) state.ty_of_var;
    prerr_endline "--> TYPE MAPPING:";
    MTMap.iter (fun ty vars ->
        prerr_endline @@ "===> " ^ (Types.pp_t (ty : Types.mono_type :> Types.r_type)) ^ " -> {" ^ 
                         (String.concat ", " @@ SSet.elements vars) ^ "}"
        ) state.var_of_ty;
    

  method private compute_interference cc_seq = 
    let ref_vars = self#walk_call_seq SSet.add SSet.empty cc_seq in
    (* now compute the congruence closure *)
    let uf = UnionFind.create () in
    SSet.iter (fun v ->
        UnionFind.add_element uf v
      ) ref_vars;
    List.iter (function 
        | Null_Assert _
        | Ptr_Assert _ -> ()
        | Close_Block -> ()
        | Copy_Action (v1,v2) -> 
          UnionFind.union uf v1 v2
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
    | Null_Assert _::t
    | Ptr_Assert _::t -> self#walk_call_seq f accum t
    | Close_Block::t -> self#walk_call_seq f accum t
    | Copy_Action _::t -> self#walk_call_seq f accum t
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

  method private all_copy_var_used call_seq = 
    let copy_vars = List.fold_left (fun accum l -> 
        match l with
        | Copy_Action (v,_) -> SSet.add v accum
        | _ -> accum
      ) SSet.empty call_seq in
    let ref_vars = self#walk_call_seq SSet.add SSet.empty call_seq in
    SSet.is_empty @@ SSet.diff copy_vars ref_vars

  initializer 
    let (init_args,_) = memo_get_fn_types (Env.crust_init_name_e(), []) in
    let empty_state = {
      var_of_ty = MTMap.empty;
      var_stack = [];
      ty_of_var = SMap.empty 
    } in
    match self#get_args empty_state init_args with
    | [t] -> init_arg_list <- t
    | [] -> failwith "No resolution found for crust_init arguments"
    | _ -> failwith "Ambiguous argument resolution found for crust_init"
end

let rec complex_drop ty = match ty with
  | #Types.simple_type -> false
  | `Tuple tl -> List.exists complex_drop tl
  | `Vec _ -> false
  | `Str -> false
  | `Fixed_Vec (_,t) -> complex_drop t
  | `Bottom 
  | `Ref _
  | `Ref_Mut _ 
  | `Ptr _
  | `Ptr_Mut _ -> true
  | `Adt_type a -> true
                  
  

let filter_drop call_seq = 
  let (dropped_types,_) =
    List.fold_left (fun (dropped_types,type_stack) api_action -> 
        match api_action with
        | Drop ->  begin
            match type_stack with
            | h::t -> (h::dropped_types,t)
            | _ -> assert false
          end
        | Copy ->
          let copied_type = List.hd type_stack in
          (dropped_types,copied_type::type_stack)
        | Fn (fn,m_args) -> 
          let (_,ret_type) = memo_get_fn_types (fn,m_args) in
          (dropped_types,ret_type::type_stack)
    ) ([],[]) call_seq in
  match dropped_types with 
  | [] -> true
  | _ -> List.exists complex_drop dropped_types

(* (not a) stub! *)
let gen_call = 
  fun pp sequence ->
  let call_seq = List.rev sequence in
  if not @@ filter_drop call_seq then () else begin
    pp#emit_sequence call_seq
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

let valid_extend state api_action = 
  match api_action with
  | Drop -> begin
    match state.t_list with
    | [] -> None
    | h::t -> 
      if List.mem h t then
        Some { state with t_list = t }
      else
        Some { t_list = t; public_types = MTSet.remove h state.public_types }
    end
  | Copy -> begin
      match state.t_list with
      | [] -> None
      | h::t -> if (match h with | #Types.simple_type -> true | _ -> false) then
          None
        else Some { state with t_list = h::state.t_list }
    end
  | Fn (fn_name,m_args) ->
    let (arg_types,ret_type) = memo_get_fn_types (fn_name,m_args) in
    match TypeUtil.get_inst state.public_types [] (arg_types : Types.mono_type list :> Types.r_type list) with
    | `Mismatch -> None
    | `Inst _ -> Some { t_list = ret_type::state.t_list; public_types = MTSet.add ret_type state.public_types }

let rec do_gen_mut mut_fn_set const_fn_set seq_len state index path f = 
(*  prerr_endline @@ "Exploring: " ^ (string_of_int index) ^ " with length -> " ^ (string_of_int seq_len) ^ " more debug -> " ^ (string_of_int @@ List.length path);*)
  if seq_len = !mut_action_len then
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
  if seq_len = !immut_action_len then
    f path
  else if index = (Array.length const_fn_set) then
    f path
  else begin
    (match valid_extend state const_fn_set.(index) with
    | Some st -> do_gen_immut const_fn_set (succ seq_len) st 0 (const_fn_set.(index)::path) f
    | None -> ());
    do_gen_immut const_fn_set (succ seq_len) state (succ index) path f
  end

let gen_call_seq pp fi_set = 
  let (mut_fn_set,const_fn_set) = 
    if !no_mut_analysis then
      (fi_set,Analysis.FISet.empty)
    else
      Analysis.FISet.partition mut_analysis fi_set
  in
  let fn_action_of_set s = List.map (fun (f,m) -> Fn (f,m)) @@ Analysis.FISet.elements s in
  let mut_fn_arr = Array.of_list @@ Drop::Copy::(fn_action_of_set mut_fn_set) in
  let const_fn_arr = Array.of_list @@ fn_action_of_set const_fn_set in
  do_gen_mut mut_fn_arr const_fn_arr 0 {
    public_types = List.fold_left (fun accum t -> MTSet.add t accum) MTSet.empty @@ init_types ();
    t_list = []
  } 0 [] (gen_call pp)

let gen_driver output_prefix test_slice pt_set pf_set = 
  let out_buffer = Buffer.create 1000 in
  let pp_rust = new rust_pp out_buffer output_prefix test_slice in
  gen_call_seq pp_rust pf_set;
  pp_rust#flush_buffer ()

let dump_api out_channel pt_set pf_set = 
  let buf = Buffer.create 1000 in
  Buffer.add_string buf "-- PUBLIC TYPES --\n";
  MTSet.iter (fun ty -> 
      (* will this work??? I hope so... *)
      Buffer.add_string buf "* ";
      Buffer.add_string buf @@ unmangle_name @@ Types.pp_t (ty : Types.mono_type :> Types.r_type);
      Buffer.add_string buf "\n"
    ) pt_set;
  Buffer.output_buffer out_channel buf;
  Buffer.clear buf;
  Buffer.add_string buf "-- PUBLIC API --\n";
  Analysis.FISet.iter (fun (fn_name,m_args) ->
      Buffer.add_string buf "* ";
      let fn_name = rust_name fn_name in
      let suffix =
        match m_args with
        | [] -> ""
        | _ -> "::<" ^ (String.concat ", " @@ List.map Types.pp_t (m_args : Types.mono_type list :> Types.r_type list)) ^ ">"
      in
      Buffer.add_string buf fn_name;
      Buffer.add_string buf suffix;
      Buffer.add_string buf "\n";
    ) pf_set;
  Buffer.output_buffer out_channel buf
