type c_types = [
  | Types.simple_type
  | `Tuple of c_types list
  | `Adt_type of c_types Types.adt_type
  | `Ptr of c_types
  | `Ptr_Mut of c_types
  | `Bottom
]

let adt_of_tuple (tl : c_types list) = {
  Types.type_name = Types.rust_tuple_name;
  Types.type_param = tl;
  Types.lifetime_param = [];
}
let rec (adt_type_name : c_types -> string) = function
  | `Adt_type a -> mangle_adt_name a
  | `Ptr t -> (adt_type_name t) ^ "_ptr"
  | `Ptr_Mut t -> "const_" ^ (adt_type_name t) ^ "_ptr"
  | `Int n -> "i" ^ string_of_int n
  | `UInt n -> "u" ^ string_of_int n
  | `Bool -> "bool"
  | `Unit -> "unit"
  | `Bottom -> "bottom"
  | `Tuple tl -> mangle_adt_name @@ adt_of_tuple tl
and mangle_adt_name t = 
  if t.Types.type_param = [] then
    t.Types.type_name ^ "_t"
  else
    t.Types.type_name ^ "_" ^
    (String.concat "_" (List.map adt_type_name t.Types.type_param))
    ^ "_t"
and tuple_name tl = mangle_adt_name @@ adt_of_tuple tl

let adt_of_inst (t_name,m_args) = 
  {
    Types.type_name = t_name;
    Types.lifetime_param = [];
    Types.type_param = m_args
  }

let c_struct_name t = 
  "__c_struct_" ^ (mangle_adt_name t)

let mangle_fn_name fn_name mono_args = 
  match mono_args with
  | [] -> fn_name
  | _ -> fn_name ^ "_" ^ (String.concat "_" (List.map adt_type_name mono_args))

let rec type_to_string : c_types -> string = function
  | `Int n -> "rs_i" ^ string_of_int n
  | `UInt n -> "rs_u" ^ string_of_int n
  | `Bool -> "rs_bool"
  | `Unit -> "rs_unit"
  | `Ptr t -> "const " ^ (type_to_string t) ^"*"
  | `Ptr_Mut t -> (type_to_string t) ^ "*"
  | `Bottom -> "rs_bottom"
  | `Tuple l -> tuple_name l
  | `Adt_type a_type -> mangle_adt_name a_type

let string_of_binop : Ir.bin_op -> string = function
  | `BiAdd -> "+"
  | `BiSub -> "-"
  | `BiMul -> "*"
  | `BiDiv -> "/"
  | `BiRem -> "%"
  | `BiAnd -> "&&"
  | `BiOr -> "||"
  | `BiBitXor -> "^"
  | `BiBitAnd -> "&"
  | `BiBitOr -> "|"
  | `BiShl -> "<<"
  | `BiShr -> ">>"
  | `BiEq -> "=="
  | `BiLt -> "<"
  | `BiLe -> "<="
  | `BiNe -> "!="
  | `BiGe -> ">"
  | `BiGt -> ">="

let string_of_unop : Ir.un_op -> string = function
  | `UnNot -> "!"
  | `UnNeg -> "-"
  | `UnDeref -> "*"

let rec c_type_of_monomorph (ct : Types.mono_type) = 
  match ct with
  | #Types.simple_type as s -> s
  | `Ptr s
  | `Ref (_,s) -> `Ptr (c_type_of_monomorph s)
  | `Ptr_Mut s
  | `Ref_Mut (_,s) -> `Ptr_Mut (c_type_of_monomorph s)
  | `Adt_type p -> 
    `Adt_type {
      Types.type_name = p.Types.type_name;
      Types.lifetime_param = [];
      Types.type_param = List.map c_type_of_monomorph p.Types.type_param
    }
  | `Tuple tl -> `Tuple (List.map c_type_of_monomorph tl)
  | `Bottom -> `Bottom

let to_monomorph_c_type (t_bindings : (string * c_types) list) t = 
  c_type_of_monomorph (Types.to_monomorph (t_bindings :> (string * Types.mono_type) list) t)

let int_sizes = [ 8; 16; 32; 64 ];;


class typedef_emitter buf = 
  object (self)
    inherit Emit.emitter buf
    method private bindings = Types.type_binding
    method emit_type_instantiation (type_name,mono_args) = 
      let type_rep = adt_of_inst (type_name,mono_args) in
      let c_name = c_struct_name type_rep in
      let r_type_rep = (`Adt_type (type_rep : c_types Types.adt_type :> Types.poly_adt_type)) in
      self#put_i @@ "struct " ^ c_name ^ " { // " ^ (Types.pp_t r_type_rep);
      self#newline ();
      self#indent ();
      if type_name = Types.rust_tuple_name then
        self#dump_tuple mono_args
      else begin
        let t_def = Hashtbl.find Env.adt_env type_name in
        match t_def with
        | `Enum_def d -> 
          let t_binding = self#bindings d.Ir.e_tparam mono_args in
          self#dump_enum t_binding d
        | `Struct_def s ->
          let t_binding = self#bindings s.Ir.s_tparam mono_args in
          self#dump_struct t_binding s
      end;
      self#dedent();
      self#put_i "};";
      self#newline ()
    method private dump_tuple mono_args = 
      let field_names = List.mapi (fun i _ -> Printf.sprintf CRep.tuple_field i) mono_args in
      let fields = List.map2 (fun f_name f_type -> 
          (f_name,f_type)
        ) field_names mono_args in
      self#dump_fields fields
    method private dump_struct t_binding (s : Ir.struct_def) =
      self#dump_fields @@ self#monomorphize_fields t_binding s.Ir.struct_fields
    method private dump_enum t_binding enum = 
      self#dump_field_def (CRep.struct_tag_field,`Int 32);
      let has_data = List.exists (fun v -> 
          v.Ir.variant_fields <> []) 
          enum.Ir.variants
      in
      if has_data then begin
        self#put_i "union ";
        self#open_block ();
        List.iteri (self#dump_variant t_binding) enum.Ir.variants;
        self#close_block ~post:" data" ();
        self#newline ~post:";" ()
      end
      else ();
    method private dump_variant t_binding tag v = 
      if v.Ir.variant_fields = [] then ()
      else begin
        let m_args = List.map (to_monomorph_c_type t_binding) v.Ir.variant_fields in
        self#put_i "struct ";
        self#open_block ();
        self#dump_tuple m_args;
        self#close_block ~post:" " ();
        self#put @@ Printf.sprintf CRep.arm_field tag;
        self#newline ~post:";" ()
      end

    method private dump_field_def ((field_name,field_type) : string * c_types) = 
      self#put_i (type_to_string field_type);
      self#put_i " ";
      self#put_i field_name;
      self#newline ~post:";" ()
    method private monomorphize_fields t_binding f_def = 
      List.map (self#monomorph_field t_binding) f_def
    method private monomorph_field t_binding (f_name,f_type) = 
      (f_name,(to_monomorph_c_type t_binding f_type))
    method private dump_fields : ((string * c_types) list) -> unit = List.iter self#dump_field_def
  end

class expr_emitter buf t_bindings = 
  object (self)
    inherit Emit.emitter buf
    method private t_string (t : Types.r_type) = 
      type_to_string @@ to_monomorph_c_type t_bindings t
    method dump_expr (expr : CRep.all_expr) = 
      match (snd expr) with
      | `Block (s,e) ->
        self#open_block ();
        List.iter self#dump_stmt s;
        self#dump_expr e;
        (match (snd e) with
         | #CRep.complex_expr -> ()
         | _ -> self#newline ~post:";" ());
        self#close_block ();
        self#newline ()
      | `Match (_,m_arm) ->
        self#put_many "else " self#dump_match m_arm
      | #CRep.simple_expr as s -> self#dump_simple_expr s
    method dump_stmt_expr = function
      | (_,`Match _)
      | (_,`Block _) as e -> self#dump_expr e
      | e -> self#dump_expr e;
        self#newline ~post:";" ()
    method dump_simple_expr = function
      | `Var s -> self#put_i s
      | `Literal l -> self#put_i l
      | `Deref (_,e) -> 
        self#put_i "(*";
        self#dump_simple_expr e;
        self#put_i ")"
      | `Address_of (_,e) ->
        self#put_i "&";
        self#dump_simple_expr e
      | `Return (_,e) ->
        self#put_i "return ";
        self#dump_simple_expr e
      | `Assignment (lhs,(_,rhs)) ->
        self#dump_simple_expr lhs;
        self#put_i " = ";
        self#dump_simple_expr rhs
      | `Cast ((_,expr),t) ->
        self#put_i "(";
        self#put @@ Printf.sprintf "(%s)" @@ self#t_string t;
        self#dump_simple_expr expr;
        self#put ")"
      | `Struct_Field (s,f) ->
        self#dump_simple_expr s;
        self#put_i ".";
        self#put f
      | `Call (fn_name,_,inst,args) ->
        let m_args = List.map (to_monomorph_c_type t_bindings) inst in
        let mangled_fname = mangle_fn_name fn_name m_args in
        self#put_i mangled_fname;
        self#put "(";
        self#put_many ", " self#dump_args args;
        self#put ")"
      | `BinOp (op,(_,rhs),(_,lhs)) ->
        self#put_i "(";
        self#dump_simple_expr rhs;
        self#put " ";
        self#put @@ string_of_binop op;
        self#put " ";
        self#dump_simple_expr lhs;
        self#put ")"
      | `UnOp (op,(_,e)) ->
        self#put_i "(";
        self#put @@ string_of_unop op;
        self#dump_simple_expr e;
        self#put ")"
    method dump_match (match_condition,match_body) = 
      self#put_i "if(";
      self#dump_expr (match_condition :> CRep.all_expr);
      self#put_i ") ";
      self#dump_stmt_expr match_body;
    method dump_stmt = function
      | `Let (v_name,r_type,expr) ->
        let m_type = to_monomorph_c_type t_bindings r_type in
        let type_string = type_to_string m_type in
        self#put_i @@ Printf.sprintf "%s %s = " type_string v_name;
        self#dump_expr (expr :> CRep.all_expr);
        self#newline ~post:";" ()
      | `Declare (v_name,r_type) ->
        let m_type = to_monomorph_c_type t_bindings r_type in
        let type_string = type_to_string m_type in
        self#put_i @@ Printf.sprintf "%s %s" type_string v_name;
        self#newline ~post:";" ()
      | `Expr e ->
        self#dump_stmt_expr e;
    method dump_args (_,e) = self#dump_simple_expr e
  end

type arg_result = 
  | Primitive of string
  (* an "a priori" value, could be a reference *)
  | Complex of string * c_types
  (* a SYNTHESIZED reference *)
  | ComplexRef of string * c_types
  (* represents a SYNTHESIZED tuple *)
  | Tuple of string * (arg_result list)

let simple_type_repr t = 
  match t with
  | `Unit -> "char"
  | `Bool -> "uint8_t"
  | `Int w ->
    Printf.sprintf "int%d_t" w
  | `UInt w -> 
    Printf.sprintf "uint%d_t" w

class driver_emission buf (public_types : c_types list) = object(self)
  inherit Emit.emitter buf
  val tcode_const_cache = Hashtbl.create 10
  val tsize_const_cache = Hashtbl.create 10
  val tvalue_array_cache = Hashtbl.create 10
  val nondet_gen_cache = Hashtbl.create 10
  val stack_name = "stack"
  val stack_type = "stack_info"
  val live_state = "live"
  val user_state = "users"
  val n_object_symbol = "N_OBJECTS"
  val stack_depth_symbol = "MAX_STACK"
  val max_actions_symbol = "N_ACTIONS"
  val stack_ptr_symbol = "stack_depth"
  val loop_index = "p_state"
  val slot_trans_symbol =  "i_of_slot"

  val borrow_field = "borrowed_from"
  val i_field = "i"
  val type_field = "type"
  val immutable_field = "immutable"
  val slot_field = "slot"

  val nondet_int_symbol = "nondet_driver_int()"

  val no_borrow_symbol = "NO_BORROW"
  val immutable_borrow_symbol = "IMMUTABLE"
  val mutable_borrow_symbol = "MUTABLE"

  method emit_driver (public_fn : (string * c_types list) list) = 
    self#emit_consts ();
    self#emit_nondets ();
    self#emit_stack_def ();
    self#emit_slot_mapping ();
    self#put_all [ "int main(int argc, char **argv) "];
    self#open_block ();
    self#emit_state ();
    self#emit_initialization ();
    self#emit_for "_action_index" max_actions_symbol (self#emit_main_loop public_fn);
    self#put_all [ "return 0;" ];
    self#newline ();
    self#close_block ()

  method private emit_block em = 
    self#open_block ();
    em ();
    self#close_block ();
    self#newline ()
  method private array_ref array_name index = 
    Printf.sprintf "%s[%s]" array_name index
  method private emit_for index bound body =
    self#put_all [
      "for(int " ; index ; " = 0; " ; index ; " < " ; bound ; "; " ; index ; "++) " 
    ];
    self#open_block ();
    body index;
    self#close_block();
    self#newline ();
  method private emit_array_assign arr_name arr_index rhs = 
    self#put_all [ (self#array_ref arr_name arr_index); " = "; rhs ];
    self#newline ~post:";" ()
  method private emit_assume cond = 
    self#put_i @@ self#assume cond;
    self#newline ()
  method private assume cond = 
    "__CPROVER_assume(" ^ cond ^ ");"
  method private emit_slot_mapping () = 
    self#put_all [ "int "; slot_trans_symbol; "(int slot, int t_code)" ];
    self#open_block();
    List.iter self#put_slot_step public_types;
    self#put_i "assert(0);";
    self#newline ();
    self#put_i "return -1;";
    self#newline ();
    self#close_block ();
    self#newline ()
  method private put_slot_step t = 
    self#put_i "if(t_code == ";
    self#put @@ Hashtbl.find tcode_const_cache t;
    self#put ") ";
    self#open_block ();
    self#put_i "return slot;";
    self#newline ();
    self#close_block ();
    self#newline ();
    self#put_i "slot += ";
    self#put @@ Hashtbl.find tsize_const_cache t;
    self#newline ~post:";" ()

  method private emit_initialization () = 
    let init_fun = Hashtbl.find Env.fn_env "crust_init" in
    let seed_tuple = to_monomorph_c_type [] init_fun.Ir.ret_type in
    let seed_types = List.filter (function
        | #Types.simple_type -> false
        | `Bottom -> false
        | _ -> true
      ) (match seed_tuple with `Tuple tl -> tl | _ -> assert false) in
    self#put_all [ type_to_string seed_tuple; " __init = crust_init();"];
    self#newline ();
    let _ = 
      List.fold_left (fun (ind,i_map) (t : c_types) ->
          let val_ind = 
            if List.mem_assoc t i_map then
              List.assoc t i_map
            else 0
          in
          let t_field = Printf.sprintf CRep.tuple_field ind in
          let t_value = "__init." ^ t_field in
          let val_ind_s = (string_of_int val_ind) in
          let t_index = self#slot_call val_ind_s t in
          let val_array = Hashtbl.find tvalue_array_cache t in
          self#emit_array_assign val_array val_ind_s t_value;
          self#emit_array_assign live_state t_index "1";
          (succ ind),((t,(succ val_ind))::i_map)
        ) (0,[]) seed_types
    in
    ()

  method private emit_define define_name define_val = 
    self#put_all [ "#define "; define_name; " "; define_val ];
    self#newline ()
  method private emit_consts () = 
    self#emit_define no_borrow_symbol "-1";
    self#emit_define immutable_borrow_symbol "0";
    self#emit_define mutable_borrow_symbol "1";
    self#emit_define max_actions_symbol "12";
    List.iteri (fun index ty ->
        let type_code = string_of_int index in
        let type_const = "TYPE_" ^ (String.uppercase @@ adt_type_name ty) in
        let type_size_const = "N_" ^ (String.uppercase @@ adt_type_name ty) in
        Hashtbl.add tcode_const_cache ty type_const;
        self#emit_define type_const type_code;
        Hashtbl.add tsize_const_cache ty type_size_const;
        self#emit_define type_size_const "3";
      ) (public_types : c_types list);
    let all_types = Hashtbl.fold (fun _ t_const accum ->
        t_const::accum
      ) tsize_const_cache [] in
    let n_object_const = "(" ^ (String.concat " + " all_types) ^ ")" in
    self#emit_define n_object_symbol n_object_const;
    self#emit_define stack_depth_symbol "12"

  method private emit_nondets () = 
    let emit_nondet t_const = 
      let t_const_uc = (t_const : Types.simple_type :> c_types) in
      let nondet_name = "nondet_" ^ (type_to_string t_const_uc) ^ "()" in
      self#put_all [ simple_type_repr t_const; " "; nondet_name; ";"];
      Hashtbl.add nondet_gen_cache t_const nondet_name;
      self#newline ()
    in
    Hashtbl.add nondet_gen_cache `Unit "0";
    List.iter (fun sizes ->
        emit_nondet (`UInt sizes);
        emit_nondet (`Int sizes)
      ) int_sizes;
    self#put_all [ simple_type_repr `Bool; " nondet_boolean()"];
    self#open_block();
    self#put_all [ simple_type_repr `Bool; " to_ret = "; (Hashtbl.find nondet_gen_cache (`Int 8)); ";"];
    self#newline ();
    self#emit_assume "to_ret == 1 || to_ret == 0";
    self#put_all [ "return to_ret;" ];
    self#newline ();
    self#close_block ();
    self#newline ();
    Hashtbl.add nondet_gen_cache `Bool "nondet_boolean()";
    self#put_all [ "int " ; nondet_int_symbol ; ";"];
    self#newline ()

  method private emit_array_def arr_type arr_name size = 
    self#put_all [ arr_type; " "; arr_name; "["; size; "];" ];
    self#newline ()
  method private slot_call slot ty = 
    let t_code = Hashtbl.find tcode_const_cache ty in
    Printf.sprintf "%s(%s, %s)" slot_trans_symbol slot t_code
  method private emit_state () = 
    let init_index = "_init_index" in
    self#emit_array_def "int" live_state n_object_symbol;
    self#emit_for init_index n_object_symbol (fun l ->
        self#emit_array_assign live_state l "0"
      );
    self#emit_array_def "int" user_state n_object_symbol;
    self#emit_for init_index n_object_symbol (fun l ->
        self#emit_array_assign user_state l "0"
      );
    self#emit_array_def stack_type stack_name stack_depth_symbol;
    List.iter (fun t ->
        let arr_type = (type_to_string t) in
        let arr_name = "state_" ^ (adt_type_name t) in
        Hashtbl.add tvalue_array_cache t arr_name;
        self#emit_array_def arr_type arr_name @@ Hashtbl.find tsize_const_cache t
      ) public_types;
    self#put_all [ "int "; stack_ptr_symbol; " = 0;" ];
    self#newline ()
  method private emit_stack_def () = 
    self#put_i "typedef struct ";
    self#open_block ();
    List.iter (fun field_name ->
        self#put_i "int ";
        self#put field_name;
        self#newline ~post:";" ()
      ) [
      borrow_field;
      immutable_field;
      type_field;
      i_field;
      slot_field
    ];
    self#close_block ();
    self#put_all [ " "; stack_type ];
    self#newline ~post:";" ()
  method private emit_constraint var upper_bound = 
    self#emit_assume @@ Printf.sprintf "%s >= 0 && %s < %s" var var upper_bound
  method private emit_bounded_int v_name upper_bound = 
    self#put_all [ "int " ; v_name ; " = " ; nondet_int_symbol ];
    self#newline ~post:";" ();
    self#emit_constraint v_name upper_bound
  method private emit_main_loop public_fn loop_index = 
    let num_actions = 1 + (List.length public_fn) in
    let action_var = "action" in
    self#emit_bounded_int action_var @@ string_of_int num_actions;
    self#emit_assume @@ "action == 0 || " ^ (stack_ptr_symbol) ^ " < " ^ stack_depth_symbol;
    self#emit_drop_action action_var;
    List.iteri (self#emit_action action_var) public_fn;
    self#newline ();
    self#emit_assertions ()

  method private coalesce_option o_list = 
    let l = List.fold_left (fun accum elem ->
        match elem with
        | Some s ->
          s @ accum
        | None -> accum
      ) [] o_list
    in
    if l = [] then None
    else Some l

  method private get_adt_drop_fn p = 
    let raw_name = 
      let adt_def = Hashtbl.find Env.adt_env p.Types.type_name in
      match adt_def with
      | `Enum_def e -> e.Ir.drop_fn
      | `Struct_def s -> s.Ir.drop_fn
    in
    match raw_name with
    | None -> None
    | Some df ->
      Some (mangle_fn_name df p.Types.type_param)

  method private collect_drop to_drop ty = 
    match ty with
    | #Types.simple_type -> None
    | `Ptr _ -> None
    | `Ptr_Mut _ -> None
    | `Bottom -> None
    | `Tuple tl ->
      self#coalesce_option @@
      List.mapi (fun i ty' ->
          let new_field = Printf.sprintf "%s.%s" to_drop @@ Printf.sprintf CRep.tuple_field i in
          self#collect_drop new_field ty'
        ) tl
    | `Adt_type p ->
      begin
        match self#get_adt_drop_fn p with
        | None -> None
        | Some df_name ->
          Some [ Printf.sprintf "%s(&(%s));" df_name to_drop ]
      end

  method private get_drop_calls slot ty = 
    match ty with
    | #Types.simple_type -> None
    | `Ptr _ -> None
    | `Ptr_Mut _ -> None
    | `Bottom -> None
    | `Tuple tl ->
      let to_drop = self#array_ref (Hashtbl.find tvalue_array_cache ty) slot in
      self#coalesce_option @@ 
      List.mapi (fun i ty' ->
          let new_field = Printf.sprintf "%s.%s" to_drop @@ Printf.sprintf CRep.tuple_field i in
          self#collect_drop new_field ty'
        ) tl
    | `Adt_type _ -> 
      let to_drop = self#array_ref (Hashtbl.find tvalue_array_cache ty) slot in
      self#collect_drop to_drop ty


  method private emit_drop_call type_value slot index ty = 
    (if index != 0 then self#put_i " else " else ());
    let type_code = Hashtbl.find tcode_const_cache ty in
    self#put_all [ "if("; type_value; " == "; type_code; ") " ];
    self#open_block ();
    begin
      match self#get_drop_calls slot ty with
      | None -> 
        self#put_all [ "// No drop action for: "; Types.pp_t (ty : c_types :> Types.r_type) ];
        self#newline ()
      | Some df ->
        List.iter (fun s -> self#put_i s; self#newline ()) df
    end;
    self#close_block ()
  method private emit_drop_action action_var = 
    self#put_all [ "if("; action_var; " == 0) " ];
    self#open_block ();
    self#emit_assume @@ Printf.sprintf "0 < %s" stack_ptr_symbol;
    let stack_symbol = self#array_ref stack_name @@ Printf.sprintf "%s - 1" stack_ptr_symbol in
    let borrowed_from_value = stack_symbol ^ "." ^ borrow_field in
    let type_value = stack_symbol ^ "." ^ type_field in
    let slot_value = stack_symbol ^ "." ^ slot_field in
    let i_value = stack_symbol ^ "." ^ i_field in
    let immutable_value = stack_symbol ^ "." ^ immutable_field in
    let borrowed_p = [ "if("; borrowed_from_value ; " != "; no_borrow_symbol ; " && " ] in
    begin
      self#put_all @@ borrowed_p @ [ immutable_value ; " == " ; immutable_borrow_symbol; ") " ];
      self#open_block ();
      self#put_all [ self#array_ref user_state borrowed_from_value; "--;" ];
      self#newline ();
      self#close_block ()
    end;
    begin
      self#put_i " else ";
      self#put_all @@ borrowed_p @ [ immutable_value ; " == "; mutable_borrow_symbol; ") " ];
      self#open_block ();
      self#emit_array_assign user_state borrowed_from_value "0";
      self#close_block ();
      self#newline ()
    end;
    self#emit_array_assign live_state i_value "0";
    List.iteri (self#emit_drop_call type_value slot_value) public_types;
    self#newline ();
    self#put_all [ stack_ptr_symbol; "--;"];
    self#newline ();
    self#close_block ()

  method private get_bounded (t : Types.simple_type) = 
    Hashtbl.find nondet_gen_cache t

  method private get_fn_def fn_name = 
    Hashtbl.find Env.fn_env fn_name
  method private emit_live_constraint slot ty const = 
    let slot_call = self#slot_call slot ty in
    let array_ref = self#array_ref live_state slot_call in
    match const with
    | `Live -> self#emit_assume @@ array_ref ^ " == 1"
    | `Dead -> self#emit_assume @@ array_ref ^ " == 0"
  method private emit_borrow_constraint slot ty borrow_mode = 
    let slot_call = self#slot_call slot ty in
    let array_ref = self#array_ref user_state slot_call in
    match borrow_mode with
    | `Immutable -> self#emit_assume @@ array_ref ^ " != -1"
    | `Mutable -> self#emit_assume @@ array_ref ^ " == 0"
  val mutable var_counter = 0
  method private emit_fresh_bounded_int size = 
    let new_var_name = Printf.sprintf "temp_%d" var_counter in
    var_counter <- var_counter + 1;
    self#emit_bounded_int new_var_name size;
    new_var_name

  method gen_ptr is_mut ty = 
    not (
      if is_mut then
        List.mem (`Ptr_Mut ty) public_types
      else
        (List.mem (`Ptr_Mut ty) public_types) ||
        (List.mem (`Ptr ty) public_types)
    )

  method private resolve_arg ty = 
    let handle_ref t is_ref mut = 
      let size_const = Hashtbl.find tsize_const_cache t in
      let src_var = self#emit_fresh_bounded_int size_const in
      self#emit_live_constraint src_var t `Live;
      self#emit_borrow_constraint src_var t mut;
      if is_ref then
        ComplexRef (src_var,t)
      else
        Complex (src_var,t)
    in
    match ty with 
    | #Types.simple_type as s ->
      let out_var = self#get_bounded s in
      Primitive out_var
    | `Ptr t when self#gen_ptr true t -> 
      handle_ref t true `Immutable
    | (`Ptr _) as t->
      handle_ref t false `Immutable
    | `Ptr_Mut t when self#gen_ptr false t ->
      handle_ref t true `Mutable
    | (`Ptr_Mut _) as t -> 
      handle_ref t false `Mutable
    | `Tuple tl when not (List.mem ty public_types) ->
      let t_name = type_to_string ty in
      let temp_name = Printf.sprintf "temp_tuple_%d" var_counter in
      var_counter <- var_counter + 1;
      self#put_all [ t_name; " "; temp_name; ";" ];
      self#newline ();
      let sub_vars = 
        List.mapi (fun i sub_ty ->
            let ret_type = self#resolve_arg sub_ty in
            self#put_all [ temp_name; "."; Printf.sprintf CRep.tuple_field i; " = " ];
            (match ret_type with
            | Tuple (rhs,_)
            | Primitive rhs -> 
              self#put rhs;
            | Complex (ind,t) ->
              let val_array = Hashtbl.find tvalue_array_cache t in
              let rhs = self#array_ref val_array ind in
              self#put rhs
            | ComplexRef (ind,t) ->
              let val_array = Hashtbl.find tvalue_array_cache t in
              let rhs = self#array_ref val_array ind in
              self#put @@ "&" ^ rhs
            );
            self#newline ~post:";" ();
            ret_type
          ) tl
      in
      Tuple (temp_name,sub_vars)
    | a -> 
      let size_const = Hashtbl.find tsize_const_cache a in
      let src_var = self#emit_fresh_bounded_int size_const in
      self#emit_live_constraint src_var a `Live;
      Complex (src_var,a)
  method private resolve_types fn_name method_args = 
    let fn_def = Hashtbl.find Env.fn_env fn_name in
    let t_bindings = Types.type_binding fn_def.Ir.fn_tparams (method_args : c_types list :> Types.mono_type list) in
    let ret_type = c_type_of_monomorph @@ Types.to_monomorph t_bindings fn_def.Ir.ret_type in
    let args = List.map snd fn_def.Ir.fn_args
               |> List.map (Types.to_monomorph t_bindings)
               |> List.map c_type_of_monomorph
    in
    (ret_type, args)
  method private emit_action action_variable iteration (fn_name,m_args) =
    let (ret_type,arg_types) = self#resolve_types fn_name m_args in
    let action_index = iteration + 1 in
    self#put_all [ " else if("; action_variable ; " == " ; (string_of_int action_index); ") " ];
    self#open_block ();
    let save_return = match ret_type with #Types.simple_type -> false | _ -> true in
    let f_name = mangle_fn_name fn_name m_args in
    let arg_results = List.map self#resolve_arg arg_types in
    let arg_strings = List.map (function
        | Complex (src_var,t) ->
          let arr_name = Hashtbl.find tvalue_array_cache t in
          self#array_ref arr_name src_var
        | ComplexRef (src_var,t) ->
          let arr_name = Hashtbl.find tvalue_array_cache t in
          let v = self#array_ref arr_name src_var in
          "&" ^ v
        | Tuple (t_name,_) -> t_name
        | Primitive s -> s
      ) arg_results in
    let arg_string = String.concat ", " arg_strings in
    begin
      if not save_return then begin
        self#put_all [ f_name; "("; arg_string ; ");" ];
        self#newline ()
      end else begin
        let target_size = Hashtbl.find tsize_const_cache ret_type in
        let target_var = self#emit_fresh_bounded_int target_size in
        self#emit_live_constraint target_var ret_type `Dead;
        let target_array = Hashtbl.find tvalue_array_cache ret_type in
        let call_target = self#array_ref target_array target_var in
        let i_slot = self#slot_call target_var ret_type in
        self#put_all [ call_target; " = "; f_name ; "(" ; arg_string ; ");"];
        self#newline ();
        self#put_all [ self#array_ref live_state i_slot; " = 1;" ];
        self#newline ();
        let (borrow_type,borrow_from) = self#emit_user_update fn_name arg_results in
        (* now update the stack *)
        let update_fields = [
          (borrow_field,borrow_from);
          (immutable_field,borrow_type);
          (slot_field,target_var);
          (i_field,i_slot);
          (type_field,(Hashtbl.find tcode_const_cache ret_type))
        ] in
        List.iter self#emit_stack update_fields;
        self#put_all [ stack_ptr_symbol; "++;"];
        self#newline ()
      end
    end;
    self#close_block ()

  method private emit_stack = 
    let stack_ref = self#array_ref stack_name stack_ptr_symbol in
    fun (s_field,s_value) ->
      self#put_all [ stack_ref; "."; s_field; " = "; s_value; ";"];
      self#newline ()

  val analysis_cache = Hashtbl.create 20;
  method private emit_user_update fn_name args = 
    let borrow_info = 
      if Hashtbl.mem analysis_cache fn_name then
        Hashtbl.find analysis_cache fn_name
      else
        let b_info = Analysis.borrow_analysis @@ self#get_fn_def fn_name in
        Hashtbl.add analysis_cache fn_name b_info;
        b_info;
    in
    let borrow_from_index arg_list index = 
      let m = List.nth arg_list index in
      match m with
      | Primitive _ -> failwith "Attempt to borrow from primitive value!"
      | Complex (src_var,t)
      | ComplexRef (src_var,t) ->
        self#slot_call src_var t
      | Tuple _ -> failwith "Attempt to borrow from a tuple!!?!?!"
    in
    let do_immutable_borrow index = 
      self#put_all [ self#array_ref user_state index; "++;" ];
      self#newline ();
      (immutable_borrow_symbol,index)
    in
    let do_mutable_borrow index = 
      self#emit_array_assign user_state index "-1";
      (mutable_borrow_symbol,index)
    in
    match borrow_info with
    | `NoBorrow -> (no_borrow_symbol,no_borrow_symbol)
    | `ImmutableBorrow i ->
      let borrow_index = borrow_from_index args i in
      do_immutable_borrow borrow_index
    | `MutableBorrow i ->
      let borrow_index = borrow_from_index args i in
      do_mutable_borrow borrow_index
    | `Tuple (i,b) -> 
      let rec unfold_tuple arg_list = function
        | `MutableBorrow i -> 
          do_mutable_borrow @@ borrow_from_index arg_list i
        | `ImmutableBorrow i ->
          do_immutable_borrow @@ borrow_from_index arg_list i
        | `Tuple (ind,borrow_info') ->
          let arg = List.nth arg_list ind in
          begin
            match arg with
            | Tuple (_,tl) -> unfold_tuple tl borrow_info'
            | _ -> failwith "Attempt to traverse tuple but non-tuple arg found!"
          end
      in
      unfold_tuple args @@ `Tuple (i,b)
      
  
      

  method private find_aliasing_types () = 
    let pointer_types = List.fold_left (fun accum s ->
        match s with
        | `Ptr t -> (`Ptr t)::accum
        | `Ptr_Mut t -> (`Ptr_Mut t)::accum
        | _ -> accum
      ) [] public_types
    in
    let mutable_pointers = List.fold_left (fun accum s ->
        match s with
        | `Ptr_Mut t -> t::accum
        | _ -> accum
      ) [] pointer_types
    in
    if mutable_pointers = [] then
      []
    else
      List.fold_left (fun accum t ->
          let mut_check = `Mut_Check (`Ptr_Mut t) in
          if List.mem (`Ptr t) pointer_types then
            let immut_check = `Im_Check ((`Ptr_Mut t),(`Ptr t)) in
            immut_check::mut_check::accum
          else
            mut_check::accum
        ) [] mutable_pointers

  method private emit_alias_check slot1 type1 slot2 type2 = 
    let index1 = self#slot_call slot1 type1 in
    let index2 = self#slot_call slot2 type2 in
    let v1 = Hashtbl.find tvalue_array_cache type1 in
    let v2 = Hashtbl.find tvalue_array_cache type2 in
    self#put_all [
      "assert(";
      self#array_ref live_state index1; " == 0 ||";
      self#array_ref live_state index2; " == 0 ||";
      self#array_ref v1 slot1; " != "; self#array_ref v2 slot2;
      ");"
    ];
    self#newline ()

  method private emit_alias_check_loop = function
    | `Im_Check (t1,t2) ->
      let outer_index = "outer_index" in
      let outer_bound = Hashtbl.find tsize_const_cache t1 in
      let inner_index = "inner_index" in
      let inner_bound = Hashtbl.find tsize_const_cache t2 in
      self#emit_for outer_index outer_bound (fun s1 ->
          self#emit_for inner_index inner_bound (fun s2 ->
              self#emit_alias_check s1 t1 s2 t2
            )
        )
    | `Mut_Check t ->
      let bound = Hashtbl.find tsize_const_cache t in
      let outer_index = "outer_index" in
      let inner_index = "inner_index" in
      self#emit_for outer_index bound (fun s1->
          self#emit_for inner_index bound (fun s2 ->
              self#put_all [ "if(" ; s1 ; " == "; s2 ; " ) { continue; }"];
              self#newline ();
              self#emit_alias_check s1 t s2 t
            )
        )


  method private emit_assertions () = 
    List.iter (self#emit_alias_check_loop) @@ self#find_aliasing_types ()

end

let simplified_ir = Hashtbl.create 10;;

let memo_get_simple_ir fn_name f_def = 
  if Hashtbl.mem simplified_ir fn_name then
    Hashtbl.find simplified_ir fn_name
  else begin
    let ir = CRep.get_simple_ir f_def.Ir.fn_body in
    Hashtbl.add simplified_ir fn_name ir;
    ir
  end

let emit_common_typedefs out_channel =
  let emit_tdef c_rep t_name = 
    Printf.fprintf out_channel "typedef %s %s;\n" c_rep t_name
  in
  let emit_type (t_const : Types.simple_type) = 
    emit_tdef (simple_type_repr t_const) @@ type_to_string (t_const :> c_types)
  in
  Printf.fprintf out_channel "#include <stdint.h>\n";
  emit_tdef "void" "rs_bottom";
  emit_type `Unit;
  emit_type `Bool;
  List.iter (fun size ->
      emit_type (`UInt size);
      emit_type (`Int size)
    ) int_sizes

let emit_typedefs out_channel inst = 
  let adt = adt_of_inst inst in
  let struct_name = c_struct_name adt in
  Printf.fprintf out_channel "typedef struct %s %s;\n" struct_name @@ mangle_adt_name adt

let sig_of_fdef fn_def mono_args = 
  let buf = Buffer.create 100 in
  let bindings = Types.type_binding fn_def.Ir.fn_tparams mono_args in
  Buffer.add_string buf @@ type_to_string @@ to_monomorph_c_type bindings fn_def.Ir.ret_type;
  Buffer.add_string buf " ";
  Buffer.add_string buf @@ mangle_fn_name fn_def.Ir.fn_name mono_args;
  Buffer.add_string buf "(";
  let rec dump_loop first l = 
    match l with
    | (a_name,a_type)::t -> 
      if not first then Buffer.add_string buf ", " else ();
      Buffer.add_string buf @@ type_to_string @@ to_monomorph_c_type bindings a_type;
      Buffer.add_string buf " ";
      Buffer.add_string buf a_name;
      dump_loop false t
    | [] -> ()
  in
  dump_loop true fn_def.Ir.fn_args;
  Buffer.add_string buf ")";
  Buffer.contents buf

let emit_fn_def out_channel buf (fn_name,mono_args) = 
  let fn_def = Hashtbl.find Env.fn_env fn_name in
  let simple_ir = memo_get_simple_ir fn_name fn_def in
  Buffer.add_string buf @@ sig_of_fdef fn_def mono_args;
  Buffer.add_string buf " ";
  let expr_emitter = new expr_emitter buf @@ Types.type_binding fn_def.Ir.fn_tparams mono_args in
  expr_emitter#dump_expr simple_ir;
  Buffer.output_buffer out_channel buf;
  Buffer.clear buf;
  ()

let emit_fsigs out_channel f_list = 
  List.iter (fun (f_name,m_args) ->
      let f_def = Hashtbl.find Env.fn_env f_name in
      Printf.fprintf out_channel "%s;\n" @@ sig_of_fdef f_def m_args
    ) f_list

(* at this point our types are still in terms of Refs, and Ptrs.
   However in C there is no such distinction. Therefore a Foo<*T> and a
   Foo<&T> will compile to the precisely the same data structure. So at
   this point we eliminate type instantiations that are
   indistinguishable, and instead work at the level raw pointers.
*)

let rec uniq_list = function
  | [] -> []
  | h::h'::t when h = h' ->
    h::(uniq_list t)
  | h::t ->
    h::(uniq_list t)

let find_dup_inst : (string * (Types.mono_type list)) list -> (string * c_types list) list = 
  fun insts ->
    let simple_inst = 
      List.map (fun (n,m_args) ->
          (n,(List.map c_type_of_monomorph m_args))
        ) insts
    in
    List.sort Pervasives.compare simple_inst
    |> uniq_list

let emit out_channel pub_type_set pub_fn_set t_set f_set = 
  let t_list = find_dup_inst @@ Analysis.TISet.elements t_set in
  let f_list = find_dup_inst @@ Analysis.FISet.elements f_set in
  let (pt_list : c_types list) = List.sort Pervasives.compare @@ List.map c_type_of_monomorph @@ Analysis.MTSet.elements pub_type_set in
  let pt_list = uniq_list pt_list in
  let pf_list = find_dup_inst @@ Analysis.FISet.elements pub_fn_set in
  emit_common_typedefs out_channel;
  List.iter (emit_typedefs out_channel) t_list;
  emit_fsigs out_channel f_list;
  let type_buffer = Buffer.create 1000 in
  let type_emitter = new typedef_emitter type_buffer in
  List.iter (fun t_inst ->
      type_emitter#emit_type_instantiation t_inst
    ) t_list;
  Buffer.output_buffer out_channel type_buffer;
  Buffer.reset type_buffer;
  let fn_buffer = Buffer.create 1000 in
  List.iter (emit_fn_def out_channel fn_buffer) f_list;
  Buffer.clear fn_buffer;
  let driver_emit = new driver_emission fn_buffer pt_list in
  driver_emit#emit_driver pf_list;
  Buffer.output_buffer out_channel fn_buffer
