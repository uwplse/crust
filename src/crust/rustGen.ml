let mut_action_len = ref 4;;
let immut_action_len = ref 2;;
let assume_ident_init = ref true;;
let no_mut_analysis = ref false;;
let skip_interfere_check = ref false;;
let skip_symm_break = ref false;;
let skip_interesting_check = ref false;;
let skip_copy_use = ref false;;

module MTSet = TypeUtil.MTSet

exception Unexpected of string;;

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

let string_of_action = function 
  | Copy -> "<COPY>"
  | Drop -> "<DROP>"
  | Fn (f_name, m_args) -> TypeUtil.string_of_inst (f_name, m_args)

let counter = ref 0;;

let fresh_name () = 
  let to_ret = "crust_test_" ^ string_of_int (!counter) in
  incr counter;
  to_ret

let unmangle_name = 
  let replace_regex = Str.regexp_string "$" in
  let digit_only = Str.regexp "^[0-9]+$" in
  fun s ->
    let tokens = List.filter (fun s -> s <> "") @@ Str.split replace_regex s in
    String.concat "::" @@ List.rev @@ snd @@ List.fold_left (fun (depth,t_list) tok ->
        if Str.string_match digit_only tok 0 then
          let depth_level = int_of_string tok in
          (if depth_level == depth then
             (pred depth_level,t_list)
           else if depth_level = depth + 1 then
             (succ depth,t_list)
           else failwith ("Illegal token depth detected: " ^ s))
        else if depth = 0 then
          (depth,tok::t_list)
        else 
          (depth,t_list)
      ) (0,[]) tokens

let split_last_segment path =
  let idx = String.rindex path ':' in
  (String.sub path 0 (idx - 1),
    String.sub path (idx + 1) (String.length path - (idx + 1)))

let rust_name fn_name =
  let fn_def = Env.EnvMap.find Env.fn_env fn_name in
  let to_transmute =
    match fn_def.Ir.fn_impl with
    | None -> fn_name
    | Some i -> i.Ir.abstract_name
  in
  unmangle_name to_transmute

let args_by_param_space params ty_args =
  let ty_space = ref [] in
  let fn_space = ref [] in
  let self_space = ref [] in
  List.iter2 (fun arg param ->
    match param.[0] with
    | 'f' -> fn_space := arg :: !fn_space
    | 't' -> ty_space := arg :: !ty_space
    | 's' -> self_space := arg :: !self_space
    | _ -> raise (Unexpected "ty param first character")
  ) ty_args params;
  (List.rev !ty_space, List.rev !fn_space, List.rev !self_space)



type method_arg = 
  | Primitive of string
  | Var of string
  | Ref of method_arg
  | Mut_Ref of method_arg
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
  | Null_Assert of [`Immut | `Mut ] * string
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
  "#![feature(unsafe_destructor, no_std)]";
  "#![feature(core, alloc, collections)]";
  "extern crate core;";
  "extern crate alloc;";
  "extern crate collections;";
  "extern crate __crust;";
  "";
]


let rec is_trivial_type ty = 
  match ty with
  | `Tuple tl -> List.for_all is_trivial_type tl
  | #Types.simple_type -> true
  | `Ref_Mut (_,t)
  | `Ref (_,t) -> is_trivial_type t
  | _ -> false

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
    let final_call_seq = self#filter_call_seq concrete_call_seqs in
    List.iter (fun cc_seq ->
        self#emit_cc_sequence init_vars cc_seq
    ) final_call_seq

  method private mkf flag f =
    if flag then List.filter f
    else (fun x -> x)

  method private filter_call_seq call_seqs = 
    (self#mkf (not !skip_copy_use) self#all_copy_var_used call_seqs)
    |> (if (!assume_ident_init && not !skip_symm_break) then self#break_symmetry else (fun x -> x))
    |> self#mkf (!assume_ident_init && not !skip_interfere_check) self#compute_interference

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
    begin
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

  method private gen_null_assert mut_flag var = 
    Null_Assert (mut_flag,var)

  method private is_dst ty = 
    match ty with
    | `Str -> true
    | `Vec _ -> true
    | _ -> false

  method private gen_assertions state out_var returned_type = 
    match returned_type with
    | (`Ref_Mut (_,t) as mut_ref) when not (self#is_dst t) ->
      (self#gen_ty_assertions state (fun v2 ->
           Ptr_Assert (`Mut,out_var,v2)) mut_ref
      ) @ (self#gen_ty_assertions state (fun v2 ->
          Ptr_Assert (`Immut,out_var,v2)) (`Ref (TypeUtil.dummy_lifetime,t))) 
        @ [ self#gen_null_assert `Mut out_var ]
    | `Ref (_,t) when not (self#is_dst t) ->
      self#gen_ty_assertions state (fun v1 ->
          Ptr_Assert (`Immut,v1,out_var)) (`Ref_Mut (TypeUtil.dummy_lifetime,t))
      @ [ self#gen_null_assert `Immut out_var ]
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
    let mk_mref ty = Mut_Ref ty in
    let mk_ref ty = Ref ty in
    let mk_tuple tl = Tuple tl in
    let build_var state ty = 
      if MTMap.mem ty state.var_of_ty then
        SSet.fold (fun v accum ->
            (Var v)::accum
          ) (MTMap.find ty state.var_of_ty) []
      else
        [] in
    fun state arg_type ->
      match arg_type with
      | #Types.simple_type as s -> [Primitive (self#nondet_fn s)]
      | `Ptr_Mut _
      | `Ptr _ -> failwith "Unsupported pointer type in public api"
      | `Bottom -> failwith "Unsupported bottom type"
      | `Str
      | `Vec _ -> []
      | (`Ref (_,t') as t) ->
        (if is_trivial_type t then [] else
           (build_var state t)) 
        @ (List.map mk_ref @@ self#resolve_argument state t')
      | (`Ref_Mut (_,t') as t) ->
        (if is_trivial_type t then [] else
           (build_var state t))
        @ (List.map mk_mref @@ self#resolve_argument state t')
      | (`Tuple tl as t) ->
        (if is_trivial_type t then [] else
           (build_var state t)) @
        List.map mk_tuple @@ self#get_args state tl
      | `Adt_type _
      | `Fixed_Vec _ ->
        build_var state arg_type

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
            let function_name = match mut_flag with | `Mut -> "crust_mref_check" | `Immut -> "crust_imref_check" in
            self#put_all [ function_name; "("; v1; ", "; v2; ");"];
            self#newline ();
            accum
          end
        | Null_Assert (mut_flag,out_var) ->
          self#put_all [
            "crust_assert("; out_var; " as *" ; 
            (match mut_flag with `Mut -> "mut" | `Immut -> "const");
            " _ as u64 != 0);"
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

  method emit_blocks bs =
    List.iteri (fun i b ->
      self#put (Printf.sprintf "fn __crust_test_%d() {\n" i);
      self#emit_expr b;
      self#put ";\n}\n\n";
    ) bs

  method emit_stmt (s : Ir.stmt) =
    match s with
    | `Let (name, _ty, Some expr) -> begin
        self#put "let mut ";
        self#put name;
        self#put " = ";
        self#emit_expr expr;
        self#put ";\n";
      end
    | `Expr expr -> begin
        self#emit_expr expr;
        self#put ";\n";
      end
    | _ -> raise (Unexpected "stmt variant")

  method emit_arm (a : Ir.match_arm) =
    let (pat, body) = a in
    self#emit_pattern pat;
    self#put " => ";
    self#emit_expr body;
    self#put ",";

  method emit_pattern (p : Ir.pattern) =
    let variant_name ty variant_name =
        match ty with
        | (`Adt_type { Types.type_name = enum_name; _ }) ->
            unmangle_name enum_name ^ "::" ^
                Str.string_after variant_name (1 + String.rindex variant_name '$')
        | _ -> raise (Unexpected "ty for enum variant")
    in
    match snd p with
    | `Bind name -> self#put name
    | `Enum (variant, _, []) -> self#put (variant_name (fst p) variant)
    | `Enum (variant, _, pats) -> begin
        self#put (variant_name (fst p) variant);
        self#put "(";
        self#put_many ", " self#emit_pattern pats;
        self#put ")";
      end
    | `Wild -> self#put "_"
    (* `Literal *)
    (* `Const *)
    | `Tuple pats -> begin
        self#put "(";
        self#put_many ", " self#emit_pattern pats;
        self#put ")";
      end
    (* `Addr_of *)
    | `Addr_of pat -> begin
        self#put "&";
        self#emit_pattern pat;
      end
    | `Ref name -> begin
        self#put "ref ";
        self#put name;
      end
    | _ -> raise (Unexpected "pat variant")

  method emit_expr (e : Ir.expr) =
    match snd e with
    | `Var name -> self#put name
    | `Literal lit ->
        (match fst e with
        | `Unit -> self#put "()";
        | `UInt _ -> self#put lit;
        | `Int _ -> self#put lit;
        | _ -> raise (Unexpected "lit type variant"))
    (* `Struct_Literal *)
    (* `Enum_Literal *)
    | `Match (expr, arms) -> begin
        self#put "match ";
        self#emit_expr expr;
        self#put "{\n";
        self#put_many "\n" self#emit_arm arms;
        self#put "}"
      end
    | `Block (stmts, expr) -> begin
        self#put "{\n";
        self#put_many "\n" self#emit_stmt stmts;
        self#put "\n";
        self#emit_expr expr;
        self#put "\n}";
      end
    | `Struct_Field (expr, field) -> begin
        self#emit_expr expr;
        self#put ".";
        self#put field;
      end
    | `Deref e' -> begin
        self#put "*";
        self#emit_expr e';
      end
    | `Address_of e' -> begin
        self#put "&";
        (match fst e with
        | `Ref_Mut _ -> self#put "mut "
        | `Ptr_Mut _ -> self#put "mut "
        | _ -> ());
        self#emit_expr e';
      end
    | `Call (name, las, tas, args) -> begin
        self#emit_call_path name tas;
        self#put "(";
        self#put_many ", " self#emit_expr args;
        self#put ")";
      end
    (* `Unsafe *)
    (* `Return *)
    (* `Assignment *)
    | `Cast e' -> begin
        self#emit_expr e';
        self#put " as ";
        self#emit_ty (fst e);
      end
    | `BinOp (op, lhs, rhs) -> begin
        let op_str =
            match op with
            | `BiEq -> "=="
            | `BiNe -> "!="
            | _ -> raise (Unexpected "bin_op variant")
        in
        self#put "(";
        self#emit_expr lhs;
        self#put (" " ^ op_str ^ " ");
        self#emit_expr rhs;
        self#put ")";
      end
    (* `UnOp *)
    | `Tuple exprs -> begin
        self#put "(";
        self#put_many ", " self#emit_expr exprs;
        self#put ")";
      end
    (* `While *)
    (* `Assign_Op *)
    (* `Vec *)
    (* `Break *)
    (* `Continue *)
    | _ -> raise (Unexpected "expr variant")

  method emit_ty (t : Types.r_type) =
    match t with
    (* | `T_Var -> raise (Unexpected "ty var") *)
    (* | `Abstract -> raise (Unexpected "abstract type") *)
    | `Adt_type { Types.type_name = t_name; Types.type_param = t_args; _ } ->
        self#put (unmangle_name t_name);
        if List.length t_args > 0 then begin
          self#put "<";
          self#put_many ", " self#emit_ty t_args;
          self#put ">";
        end;
    | `Ref (_, ty) -> begin self#put "&"; self#emit_ty ty end
    | `Ref_Mut (_, ty) -> begin self#put "&mut "; self#emit_ty ty end
    | `Ptr ty -> begin self#put "*const "; self#emit_ty ty end
    | `Ptr_Mut ty -> begin self#put "*mut "; self#emit_ty ty end
    | `Int `Ptr_Size -> self#put "isize"
    | `Int (`Bit_Size n) -> begin self#put "i"; self#put (string_of_int n) end
    | `UInt `Ptr_Size -> self#put "usize"
    | `UInt (`Bit_Size n) -> begin self#put "u"; self#put (string_of_int n) end
    | `Bool -> self#put "bool"
    | `Unit -> self#put "()"
    | `Float n -> begin self#put "f"; self#put (string_of_int n) end
    | `Char -> self#put "char"
    | `Tuple tys -> begin
        self#put "(";
        self#put_many ", " self#emit_ty tys;
        self#put ")";
      end
    (* NB: Actually emit "_" (ty_infer) instead of "!" *)
    | `Bottom -> self#put "_"
    | `Fixed_Vec (n, ty) -> begin
        self#put "[";
        self#emit_ty ty;
        self#put "; ";
        self#put (string_of_int n);
        self#put "]";
      end
    | `Str -> self#put "str"
    | `Vec ty -> begin
        self#put "[";
        self#emit_ty ty;
        self#put "]";
      end
    | _ -> self#put "wat"

  method emit_call_path fn_name ty_args =
    if String.compare fn_name "__crust$nondet" == 0
    then self#put "__crust::nondet"
    else if String.compare fn_name "__crust$assert" == 0
    then self#put "__crust::assert"
    else if String.compare fn_name "__crust$assume" == 0
    then self#put "__crust::assume"
    else if String.compare fn_name "__crust$unreachable" == 0
    then self#put "__crust::unreachable"
    else if String.compare fn_name "drop_glue" == 0
    then self#put "__crust::drop_glue"
    else begin
      let fn_def = Env.EnvMap.find Env.fn_env fn_name in
      match fn_def.Ir.fn_impl with
      | None -> begin
        let (ty_space, fn_space, self_space) = args_by_param_space fn_def.Ir.fn_tparams ty_args in
        (if List.length ty_space == 0
          then self#put (unmangle_name fn_name)
          else begin
            let (ty_name, fn_basename) = split_last_segment (unmangle_name fn_name) in
            self#put ty_name;
            self#put "::<";
            self#put_many ", " self#emit_ty ty_space;
            self#put ">";
            self#put "::";
            self#put fn_basename;
          end);
        (if List.length fn_space != 0
          then begin
            self#put "::<";
            self#put_many ", " self#emit_ty fn_space;
            self#put ">";
          end else ());
      end
      | Some i -> begin
        let afn_def = Env.EnvMap.find Env.abstract_fn_env i.Ir.abstract_name in
        let afn_ty_args = TypeUtil.subst_tys fn_def.Ir.fn_tparams ty_args
              (i.Ir.i_types @ [i.Ir.i_self]) in
        let (ty_space, fn_space, self_space) = 
          args_by_param_space afn_def.Ir.afn_tparams afn_ty_args in
        let (ty_name, fn_basename) = split_last_segment (unmangle_name afn_def.Ir.afn_name) in
        Printf.printf "name %s -> %s :: %s\n" afn_def.Ir.afn_name ty_name fn_basename;
        self#put "<";
        self#put_many ", " self#emit_ty self_space;
        self#put " as ";
        self#put ty_name;
        (if List.length ty_space > 0
          then begin
            self#put "<";
            self#put_many ", " self#emit_ty ty_space;
            self#put ">";
          end else ());
        self#put ">::";
        self#put fn_basename;
      end
    end

  method private emit_arg arg = 
    match arg with
    | Primitive s -> self#put s
    | Var v -> self#put v
    | Ref v -> 
      self#put_i "&";
      self#emit_arg v
    | Mut_Ref v -> 
      self#put_i "&mut "; 
      self#emit_arg v
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
        | Var v -> f v accum
        | Ref v -> self#walk_args f accum [v]
        | Mut_Ref v -> self#walk_args f accum [v]
        | Tuple tl -> self#walk_args f accum tl
      ) accum m_list

  method private is_symmetric vc1 vc2 =
    let rec is_symmetric_loop sym_map s1 s2 =
      match s1,s2 with
      | ((p1,h1)::t1),((p2,h2)::t2) when p1 = p2 -> 
        if SMap.mem h1 sym_map then
          if (SMap.find h1 sym_map) = h2 then
            is_symmetric_loop sym_map t1 t2
          else
            false
        else
          is_symmetric_loop (SMap.add h1 h2 sym_map) t1 t2
      | [],[] -> true
      | _,_ -> false
    in
    is_symmetric_loop SMap.empty vc1 vc2

  method private collect_args accum method_list = 
    List.fold_left (self#collect_var []) accum method_list

  method private collect_var path accum arg = 
    match arg with
    | Primitive _ -> accum
    | Var s -> (path,s)::accum
    | Ref m ->
      self#collect_var (`Ref::path) accum m
    | Mut_Ref m ->
      self#collect_var (`Mut_Ref::path) accum m
    | Tuple tl ->
      List.fold_left (self#collect_var (`Tuple::path)) accum tl

  method private build_var_choice accum cc = 
    match cc with
    | [] -> accum
    | Open_Block (_,_,_,args)::t -> self#build_var_choice (self#collect_args accum args) t
    | Null_Assert _::t
    | Close_Block::t
    | Copy_Action (_,_)::t
    | Ptr_Assert (_,_,_)::t -> self#build_var_choice accum t
            

  method private break_symmetry cc_seqs =
    let cc_vc = List.map (fun cc ->
        cc,self#build_var_choice [] cc
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

    (*
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
    *)
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

let compute_gen fn_set = 
  Analysis.FISet.fold (fun fn_inst accum ->
      let (_,ret_type) = memo_get_fn_types fn_inst in
      if is_trivial_type ret_type then accum 
      else if MTMap.mem ret_type accum then
        MTMap.add ret_type (Analysis.FISet.add fn_inst @@ MTMap.find ret_type accum) accum
      else
        MTMap.add ret_type (Analysis.FISet.singleton fn_inst) accum
    ) fn_set MTMap.empty

let rec find_deps gen_funcs fn_inst = 
  let rec maybe_add ty accum = 
    if MTMap.mem ty gen_funcs then
      Analysis.FISet.union accum @@ MTMap.find ty gen_funcs
    else
      accum
  and collect_gen accum ty = 
    match ty with
    | `Str
    | `Vec _
    | `Bottom
    | #Types.simple_type -> accum
    | `Fixed_Vec _ -> maybe_add ty accum
    | `Ref_Mut (_,ty')
    | `Ref (_,ty') -> maybe_add ty @@ collect_gen accum ty'
    | `Ptr _
    | `Ptr_Mut _ -> maybe_add ty accum
    | `Adt_type _ -> maybe_add ty accum
    | `Tuple tl ->
      maybe_add ty @@ List.fold_left collect_gen accum tl
  in
  let (arg_types,_) = memo_get_fn_types fn_inst in
  List.fold_left collect_gen Analysis.FISet.empty arg_types

let rec build_deps gen_funcs to_analyze = 
  let rec build_deps_aux accum to_analyze = 
    Analysis.FISet.fold (fun fn_inst accum ->
        if Analysis.FISet.mem fn_inst accum then
          accum
        else begin
          (* builds the functions that generate the types used in this functions args *)
          let dep_fun = find_deps gen_funcs fn_inst in
          (* pull in their deps too *)
          build_deps_aux (Analysis.FISet.add fn_inst accum) dep_fun
        end
      ) to_analyze accum
  in
  build_deps_aux Analysis.FISet.empty to_analyze

let rec filter_interesting = 
  let fn_exceptions = Str.regexp "^\\(core\\$num\\$\\(Unsigned\\)?Int\\|core\\$mem\\$size_of\\)" in
  let mem_interesting = Hashtbl.create 100 in
  let rec is_interesting pub_set fn_inst = 
    if Hashtbl.mem mem_interesting fn_inst then
      Hashtbl.find mem_interesting fn_inst
    else begin
      let (fn_name,m_args) = fn_inst in
      let fn_def = Env.EnvMap.find Env.fn_env fn_name in
      let t_bindings = Types.type_binding fn_def.Ir.fn_tparams m_args in
      let has_unsafe = find_interesting_expr pub_set t_bindings fn_def.Ir.fn_body in
      Hashtbl.add mem_interesting fn_inst has_unsafe;
      has_unsafe
    end
  (* you must be thinking: there is no way this works... but it DOES *)
  and is_abort_block = function
    | (`Let (_,_,Some (_,(`Call ("core$intrinsics$abort",[],[],[]))))::_,_) -> true
    | _ -> false
  and find_interesting_expr pub_set t_bindings (_,expr) = 
    match expr with
    (* terminal nodes *)
    | `Literal _
    | `Var _ -> false
    | `Unsafe (s,e) -> 
      not @@ is_abort_block (s,e)
    (* single nodes *)
    | `Struct_Field (e,_)
    | `Deref e
    | `Address_of e
    | `Return e
    | `Cast e
    | `UnOp (_,e) -> find_interesting_expr pub_set t_bindings e
    (* list nodes *)
    | `Enum_Literal (_,_,el)
    | `Vec el
    | `Tuple el -> List.exists (find_interesting_expr pub_set t_bindings) el
    (* struct lit *)
    | `Struct_Literal sl ->
      List.exists (fun (_,e) -> find_interesting_expr pub_set t_bindings e) sl
    (* binary ops *)
    | `BinOp (_,e1,e2)
    | `While (e1,e2)
    | `Assignment (e1,e2)
    | `Assign_Op (_,e1,e2) -> List.exists (find_interesting_expr pub_set t_bindings) [e1;e2]
    (* complex ops *)
    | `Block (s,e1) -> 
      List.exists (function 
          | `Let (_,_,Some e1) -> find_interesting_expr pub_set t_bindings e1
          | `Let (_,_,None) -> false
          | `Expr e -> find_interesting_expr pub_set t_bindings e
        ) s ||
      find_interesting_expr pub_set t_bindings e1
    | `Match (e,m_e) ->
      find_interesting_expr pub_set t_bindings e ||
      List.exists (fun (_,e) -> find_interesting_expr pub_set t_bindings e) m_e
    | `Call (fn_name,_,t_list,el) ->
      List.exists (find_interesting_expr pub_set t_bindings) el ||
      if Intrinsics.is_crust_intrinsic fn_name ||
         Intrinsics.is_intrinsic_fn fn_name ||
         fn_name = "drop_glue" then
        false
      else if Str.string_match fn_exceptions fn_name 0 then
        false
      else
        let arg_types = List.map (fun (ty,_) -> TypeUtil.to_monomorph t_bindings ty) el in
        let m_args = List.map (TypeUtil.to_monomorph t_bindings) t_list in
        let call_inst = 
          if Env.is_abstract_fn fn_name then
            let (res_args,res_name) = Analysis.resolve_abstract_fn fn_name m_args arg_types in
            (res_name,res_args)
          else
            (fn_name,m_args)
        in
        (* it doesn't matter if the function we call is interesting if it's in the public API *)
        if Analysis.FISet.mem call_inst pub_set then false else
          is_interesting pub_set call_inst 
  in
  let op_regex = Str.regexp @@ "^" ^ (Str.quote "core$ops$") in
  fun gen_func fn_set ->
    let interesting_fn = Analysis.FISet.filter (fun fi ->
        is_interesting fn_set fi
      ) fn_set in
    (* now collect the dependency functions for our interesting functions *)
    let core_functions = Analysis.FISet.filter (fun (fn_name,_) ->
        fn_name = "core$option$Option$1$T$1$$unwrap" ||
        let { Ir.fn_impl = impl; _ } = Env.EnvMap.find Env.fn_env fn_name in
         match impl with
         | None -> false
         | Some { Ir.abstract_name = a; _ } ->
           Str.string_match op_regex a 0 &&
           a <> "core$ops$Drop$drop"
      ) fn_set in
    Analysis.FISet.union 
      (build_deps gen_func interesting_fn)
      core_functions

let rec has_unsafe_type ty = match ty with
  | `Fixed_Vec (_,t)
  | `Ref_Mut (_,t)
  | `Ptr_Mut t
  | `Ptr t
  | `Ref (_,t) -> has_unsafe_type t

  | `Tuple tl -> List.exists has_unsafe_type tl
                   
  | `Adt_type { Types.type_name = "core$cell$UnsafeCell"; _ } ->
    true
  | `Adt_type { Types.type_name = t_name; Types.type_param = t_args; _ } ->
    begin
      match Env.EnvMap.find Env.adt_env t_name with
      | `Enum_def { Ir.e_tparam = t_params; Ir.variants = var; _ } ->
        let t_binding = Types.type_binding t_params t_args in
        List.exists (fun { Ir.variant_fields = fields; _ } ->
            List.exists has_unsafe_type @@ List.map (TypeUtil.to_monomorph t_binding) fields
          ) var
      | `Struct_def { Ir.s_tparam = t_params; Ir.struct_fields = fields; _ } ->
        let t_binding = Types.type_binding t_params t_args in
        List.exists has_unsafe_type @@ List.map (TypeUtil.to_monomorph t_binding) @@ List.map snd fields
    end  
  | #Types.simple_type -> false
  | `Vec _
  | `Bottom -> false
  | `Str -> false

let mut_analysis gen_funcs fn_set = 
  let rec has_mut_ref = function
    | `Ref_Mut _ -> true
    | `Ref (_,t) -> has_mut_ref t
    | #Types.simple_type -> false
    | `Tuple tl -> List.exists has_mut_ref tl
    | _ -> false
  in
  let mut_fn = Analysis.FISet.fold (fun fn_inst accum ->
      let (arg_types,_) = memo_get_fn_types fn_inst in
      if List.exists has_mut_ref arg_types ||
         List.exists has_unsafe_type arg_types then
        Analysis.FISet.add fn_inst accum
      else 
        accum
    ) fn_set Analysis.FISet.empty in
  let mut_fn = build_deps gen_funcs mut_fn in
  (mut_fn,Analysis.FISet.diff fn_set mut_fn)

let dump_state s = 
  let f = String.concat ", " @@ List.map Types.pp_t (s.t_list : Types.mono_type list :> Types.r_type list) in
  let t_set = String.concat ", " @@ List.map Types.pp_t @@ (MTSet.elements s.public_types : Types.mono_type list :> Types.r_type list) in
  prerr_endline @@ "Type stack [ " ^ f ^ " ]";
  prerr_endline @@ "Type set { " ^ t_set ^ " }"

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
  if seq_len = !mut_action_len then
    ()
  else if index = (Array.length mut_fn_set) then
    ()
  else begin
    (match valid_extend state mut_fn_set.(index) with
      | None -> ()
      | Some st -> begin
          let new_path = mut_fn_set.(index)::path in
          do_gen_immut const_fn_set 0 st 0 new_path f;
          f new_path;
          do_gen_mut mut_fn_set const_fn_set (succ seq_len) st 0 new_path f
        end
    );
    do_gen_mut mut_fn_set const_fn_set seq_len state (succ index) path f
  end
and do_gen_immut const_fn_set seq_len state index path f =
  if seq_len = !immut_action_len then
    ()
  else if index = (Array.length const_fn_set) then
    ()
  else begin
    (match valid_extend state const_fn_set.(index) with
    | Some st -> 
      let new_path = (const_fn_set.(index)::path) in
      f new_path;
      do_gen_immut const_fn_set (succ seq_len) st 0 new_path f
    | None -> ());
    do_gen_immut const_fn_set seq_len state (succ index) path f
  end

let is_fn_pub fn_name = 
  let x = Env.EnvMap.find Env.fn_env fn_name in
  match x.Ir.fn_vis with
  | `Private -> false
  | `Public -> true

let infer_api_only = ref false

let gen_call_seq pp fi_set = 
  let gen_functions = compute_gen fi_set in
  let fi_set = if !skip_interesting_check then fi_set else
      filter_interesting gen_functions fi_set in
  let (mut_fn_set,const_fn_set) = 
    if !no_mut_analysis then
      (fi_set,Analysis.FISet.empty)
    else
      mut_analysis (compute_gen fi_set) fi_set
  in
  let fn_action_of_set s = List.map (fun (f,m) -> Fn (f,m)) @@ Analysis.FISet.elements s in
  let mut_fn_arr = Array.of_list @@ Drop::Copy::(fn_action_of_set mut_fn_set) in
  let const_fn_arr = Array.of_list @@ fn_action_of_set const_fn_set in
  if !infer_api_only then begin
    let dump_action_list = Array.iteri (fun i act ->
        prerr_endline @@ (string_of_int i) ^ " -> " ^ (string_of_action act)
      ) in
    prerr_endline "== MUTABLE ACTIONS";
    dump_action_list mut_fn_arr;
    prerr_endline "== IMMUTABLE ACTIONS";
    dump_action_list const_fn_arr;
  end else
    do_gen_mut mut_fn_arr const_fn_arr 0 {
      public_types = List.fold_left (fun accum t -> MTSet.add t accum) MTSet.empty @@ init_types ();
      t_list = []
    } 0 [] (gen_call pp)

let gen_driver output_prefix test_slice = 
  let out_buffer = Buffer.create 1000 in
  let pp_rust = new rust_pp out_buffer output_prefix test_slice in
  (* gen_call_seq pp_rust pf_set; *)
  Printf.printf "%d items\n" (List.length !Env.driver_env);
  pp_rust#emit_blocks !Env.driver_env;
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
