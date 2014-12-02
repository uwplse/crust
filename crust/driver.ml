module type Compilation = sig
  type c_types = [
    | Types.simple_type
    | `Tuple of c_types list
    | `Adt_type of c_types Types.adt_type
    | `Ptr of c_types
    | `Ptr_Mut of c_types
    | `Bottom
  ]
  val adt_type_name : c_types -> string
  val type_to_string : c_types -> string
  val simple_type_repr : Types.simple_type -> string
  val to_monomorph_c_type : (string * c_types) list -> Types.r_type -> c_types
  val int_sizes : int list
  val mangle_fn_name : string -> c_types list -> string
end

module DriverF(COMP : Compilation) = struct


  type arg_result = 
    | Primitive of string
    (* an "a priori" value, could be a reference *)
    | Complex of string * COMP.c_types
    (* a SYNTHESIZED reference *)
    | ComplexRef of string * COMP.c_types
    (* represents a SYNTHESIZED tuple *)
    | Tuple of string * (arg_result list)
                      
  class driver_emission buf (public_types : COMP.c_types list) = object(self)
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

    method emit_driver (public_fn : (string * COMP.c_types list) list) = 
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
      let seed_tuple = COMP.to_monomorph_c_type [] init_fun.Ir.ret_type in
      let seed_types = List.filter (function
          | #Types.simple_type -> false
          | `Bottom -> false
          | _ -> true
        ) (match seed_tuple with `Tuple tl -> tl | _ -> assert false) in
      self#put_all [ COMP.type_to_string seed_tuple; " __init = crust_init();"];
      self#newline ();
      let _ = 
        List.fold_left (fun (ind,i_map) (t : COMP.c_types) ->
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
          let type_const = "TYPE_" ^ (String.uppercase @@ COMP.adt_type_name ty) in
          let type_size_const = "N_" ^ (String.uppercase @@ COMP.adt_type_name ty) in
          Hashtbl.add tcode_const_cache ty type_const;
          self#emit_define type_const type_code;
          Hashtbl.add tsize_const_cache ty type_size_const;
          self#emit_define type_size_const "3";
        ) (public_types : COMP.c_types list);
      let all_types = Hashtbl.fold (fun _ t_const accum ->
          t_const::accum
        ) tsize_const_cache [] in
      let n_object_const = "(" ^ (String.concat " + " all_types) ^ ")" in
      self#emit_define n_object_symbol n_object_const;
      self#emit_define stack_depth_symbol "12"

    method private emit_nondets () = 
      let emit_nondet t_const = 
        let t_const_uc = (t_const : Types.simple_type :> COMP.c_types) in
        let nondet_name = "nondet_" ^ (COMP.type_to_string t_const_uc) ^ "()" in
        self#put_all [ COMP.simple_type_repr t_const; " "; nondet_name; ";"];
        Hashtbl.add nondet_gen_cache t_const nondet_name;
        self#newline ()
      in
      Hashtbl.add nondet_gen_cache `Unit "0";
      List.iter (fun sizes ->
          emit_nondet (`UInt sizes);
          emit_nondet (`Int sizes)
        ) COMP.int_sizes;
      self#put_all [ COMP.simple_type_repr `Bool; " nondet_boolean()"];
      self#open_block();
      self#put_all [ COMP.simple_type_repr `Bool; " to_ret = "; (Hashtbl.find nondet_gen_cache (`Int 8)); ";"];
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
          let arr_type = (COMP.type_to_string t) in
          let arr_name = "state_" ^ (COMP.adt_type_name t) in
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
        Some (COMP.mangle_fn_name df p.Types.type_param)

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
          self#put_all [ "// No drop action for: "; Types.pp_t (ty : COMP.c_types :> Types.r_type) ];
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

    method private gen_ptr is_mut ty = 
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
        let t_name = COMP.type_to_string ty in
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
      let t_bindings = Types.type_binding fn_def.Ir.fn_tparams method_args in
      let ret_type = COMP.to_monomorph_c_type t_bindings fn_def.Ir.ret_type in
      let args = List.map snd fn_def.Ir.fn_args
                 |> List.map (COMP.to_monomorph_c_type t_bindings)
      in
      (ret_type, args)
    method private emit_action action_variable iteration (fn_name,m_args) =
      let (ret_type,arg_types) = self#resolve_types fn_name m_args in
      let action_index = iteration + 1 in
      self#put_all [ " else if("; action_variable ; " == " ; (string_of_int action_index); ") " ];
      self#open_block ();
      let save_return = match ret_type with #Types.simple_type -> false | _ -> true in
      let f_name = COMP.mangle_fn_name fn_name m_args in
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
end
