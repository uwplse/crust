type c_types = [
  | Types.simple_type
  | `Tuple of c_types list
  | `Adt_type of c_types Types.adt_type
  | `Ptr of c_types
  | `Ptr_Mut of c_types
  | `Bottom
  | `Fixed_Vec of int * c_types
]

type c_inst_flag = [ `Tuple | `Adt of string | `Fixed_Vec of int ];;

let tuple_name = "__rust_tuple_"

let rec adt_type_name = function
  | `Adt_type a -> mangle_adt_name a
  | `Ptr t -> (adt_type_name t) ^ "_ptr"
  | `Ptr_Mut t -> "const_" ^ (adt_type_name t) ^ "_ptr"
  | `Int n -> "i" ^ Types.string_of_intsize n
  | `UInt n -> "u" ^ Types.string_of_intsize n
  | `Char -> "c"
  | `Float i -> "f" ^ (string_of_int i)
  | `Bool -> "bool"
  | `Unit -> "unit"
  | `Bottom -> "bottom"
  | `Tuple tl -> mangle_tuple_name tl
  | `Fixed_Vec (n,t) -> "v" ^ (string_of_int n) ^"_" ^ (adt_type_name t)
  | `Ref_Mut (_,t) -> "refmut_" ^ (adt_type_name t)
  | `Ref (_,t) -> "ref_" ^ (adt_type_name t)
  | `Str -> "str"
  | `Vec t -> "vec_" ^ (adt_type_name t)
and mangle_adt_name t = 
  if t.Types.type_param = [] then
    t.Types.type_name
  else
    t.Types.type_name ^ "_" ^
    (String.concat "_" (List.map adt_type_name t.Types.type_param))
and mangle_tuple_name tl = 
  tuple_name ^ (String.concat "_" @@ List.map adt_type_name tl)


let type_of_inst (inst_flag,m_args) = 
  match inst_flag with
  | `Tuple -> `Tuple m_args
  | `Fixed_Vec n -> (match m_args with
      | [t] -> `Fixed_Vec (n,t)
      | _ -> assert false
    )
  | `Adt a ->
    `Adt_type {
      Types.type_name = a;
      Types.lifetime_param = [];
      Types.type_param = m_args
    }

let c_struct_name t = 
  "__c_struct_" ^ t

let mangle_fn_name fn_name mono_args = 
  match mono_args with
  | [] -> fn_name
  | _ -> fn_name ^ "_" ^ (String.concat "_" (List.map adt_type_name mono_args))

let rec type_to_string : c_types -> string = function
  | `Int n -> "rs_i" ^ Types.string_of_intsize n
  | `UInt n -> "rs_u" ^ Types.string_of_intsize n
  | `Bool -> "rs_bool"
  | `Unit -> "rs_unit"
  | `Float i -> "rs_f" ^ (string_of_int i)
  | `Char -> "rs_char"
  | `Ptr t -> "const " ^ (type_to_string t) ^"*"
  | `Ptr_Mut t -> (type_to_string t) ^ "*"
  | `Bottom -> "rs_bottom"
  | `Tuple l -> mangle_tuple_name l
  | `Adt_type a_type -> mangle_adt_name a_type
  | `Fixed_Vec (n,t) -> "rs_f" ^ (string_of_int n) ^ (adt_type_name t)


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
  | `BiGe -> ">="
  | `BiGt -> ">"

let string_of_unop : Ir.un_op -> string = function
  | `UnNot -> "!"
  | `UnNeg -> "-"
  | `UnDeref -> "*"

let slice_len_type = `UInt (`Bit_Size 32);;
let char_data_type = `UInt (`Bit_Size 8);;

let rec c_type_of_monomorph mono_type = 
  TypeUtil.handle_dst 
    (fun mut t -> 
       if mut then
         `Tuple [`Ptr_Mut (c_type_of_monomorph t); slice_len_type]
       else
         `Tuple [`Ptr_Mut (c_type_of_monomorph t); slice_len_type]
    )
    (fun mut ->
       if mut then
         `Tuple [`Ptr_Mut char_data_type; slice_len_type]
       else
         `Tuple [`Ptr char_data_type; slice_len_type]
    )
    (function
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
      | `Vec t -> raise TypeUtil.StrayDST
      | `Fixed_Vec (n,t) -> `Fixed_Vec (n,c_type_of_monomorph t)
      | `Str -> raise TypeUtil.StrayDST
    ) mono_type

let to_monomorph_c_type (t_bindings : (string * Types.mono_type) list) t = 
  c_type_of_monomorph (TypeUtil.to_monomorph t_bindings t)

let int_sizes = [ 8; 16; 32; 64 ];;

class typedef_emitter buf = 
  object (self)
    inherit Emit.emitter buf
    method private bindings s t = (Types.type_binding s t : (string * c_types) list :> (string * Types.mono_type) list)
    method emit_type_instantiation ((inst_flag,mono_args) : (c_inst_flag * (c_types list))) = 
      let type_rep = type_of_inst (inst_flag,mono_args) in
      let type_name = type_to_string type_rep in
      let r_type_rep = (type_rep : c_types :> Types.r_type) in
      match inst_flag with
      (* fixed vec ruins goddamn everything *)
      | `Fixed_Vec n ->
        let vec_type = type_to_string (List.hd mono_args) in
        self#put_all [ "typedef " ; vec_type ; " "; type_name; "[" ; (string_of_int n); "];" ];
        self#newline ()
      | `Tuple as i
      | (`Adt _ as i) -> begin
        let c_name = c_struct_name @@ type_name in
        self#put_i @@ "struct " ^ c_name ^ " { // " ^ (Types.pp_t r_type_rep);
        self#newline ();
        self#indent ();
        (match i with
        | `Tuple -> self#dump_tuple mono_args
        | `Adt type_name -> begin
            let t_def = Env.EnvMap.find Env.adt_env type_name in
            match t_def with
            | `Enum_def d -> 
              let t_binding = self#bindings d.Ir.e_tparam mono_args in
              self#dump_enum t_binding d
            | `Struct_def s ->
              let t_binding = self#bindings s.Ir.s_tparam mono_args in
              self#dump_struct t_binding s
          end);
        self#dedent();
        self#put_i "};"
        end;
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
      self#dump_field_def (CRep.struct_tag_field,(CRep.struct_tag_type :> c_types));
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

class expr_emitter buf (t_bindings : (string * Types.mono_type) list) = 
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
      | `While (cond,expr) ->
        self#put_i "while(";
        self#dump_simple_expr @@ snd cond;
        self#put ") ";
        self#open_block ();
        self#dump_expr expr;
        self#close_block ();
        self#newline ()
      | `Return (_,e) ->
        self#put_i "return ";
        self#dump_simple_expr e;
        self#newline ~post:";" ()
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
      | `Assignment (lhs,(_,rhs)) ->
        self#dump_simple_expr lhs;
        self#put_i " = ";
        self#dump_simple_expr rhs
      | `Assign_Op (op,e1,(_,e2)) ->
        self#dump_simple_expr e1;
        self#put_all [ string_of_binop op; "=" ];
        self#dump_simple_expr e2
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
        let mono_args = List.map (TypeUtil.to_monomorph t_bindings) inst in
        let mangled_fname = mangle_fn_name fn_name mono_args in
        if Intrinsics.is_intrinsic_fn fn_name then
          self#handle_intrinsic fn_name mangled_fname (List.map c_type_of_monomorph  mono_args) args
        else if Intrinsics.is_crust_intrinsic fn_name then
          let method_name = Intrinsics.intrinsic_name fn_name in
          self#put_all [ method_name; "(" ];
          self#put_many ", " self#dump_args args;
          self#put ")"
        else if Env.is_abstract_fn fn_name then begin
          let arg_types = List.map (fun (t,_) -> 
              (TypeUtil.to_monomorph t_bindings t)
            ) args in
          let (mono_args,resolved_name) = Analysis.resolve_abstract_fn fn_name arg_types in
          let mangled_name' = mangle_fn_name resolved_name @@ List.map (to_monomorph_c_type t_bindings) (mono_args :> Types.r_type list) in
          begin
            self#put_all [ mangled_name'; "(" ];
            self#put_many ", " self#dump_args args;
            self#put ")"
          end
        end 
        else if fn_name = "drop_glue" then
          self#handle_drop fn_name  mono_args args
        else begin
          self#put_i  mangled_fname;
          self#put "(";
          self#put_many ", " self#dump_args args;
          self#put ")"
        end
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
    method handle_drop fn_name m_args args = 
      let arg_buf = Buffer.create 100 in
      self#put_i "(";
      let arg_string = match args with
        | [(_,a)] -> self#with_intercept ~b:arg_buf (fun () -> self#dump_simple_expr a)
        | _ -> failwith @@ "Wrong arity to drop_glue, expected 1, found " ^ (string_of_int @@ List.length args)
      in
      let drop_type = match m_args with
        | [t] -> t
        | _ -> assert false
      in
      self#drop_type drop_type arg_string;
      self#put "0)"
    method drop_type (ty : Types.mono_type) arg_string = 
      match ty with
      | `Tuple tl -> 
        List.iteri (fun i ty' ->
            self#drop_type ty' (Printf.sprintf "%s.%s" arg_string (Printf.sprintf CRep.tuple_field i) )
          ) tl
      | `Adt_type a -> begin
        match Env.get_adt_drop a.Types.type_name with
        | None -> ()
        | Some df ->
          let mangled_name = mangle_fn_name df a.Types.type_param in
          self#put_all [ mangled_name; "("; arg_string; ")," ]
        end
      | _ -> ()
    method handle_intrinsic fn_name (mangled_fn_name: string) m_args args =
      let arg_buf = Buffer.create 100 in
      self#put_i "";
      let arg_strings = List.map (fun (_,expr) ->
          self#with_intercept ~b:arg_buf (fun () -> self#dump_simple_expr expr)
        ) args in
      let t_strings = List.map type_to_string m_args in
      Intrinsics.emit_intrinsic_call fn_name mangled_fn_name t_strings arg_strings buf
    method dump_match (match_condition,match_body) = 
      self#put_i "if(";
      self#dump_expr (match_condition :> CRep.all_expr);
      self#put_i ") ";
      let is_block = match match_body with
        | (_,`Block _) -> true
        | _ -> false
      in
      if not is_block then self#open_block () else ();
      self#dump_stmt_expr match_body;
      if not is_block then (self#close_block (); self#newline ()) else ()
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
      | `Vec_Init (var_name,vec_type,init_expr) ->
        self#put_all [ self#t_string vec_type; " "; var_name; " = {"];
        self#put_many ", " self#dump_expr (init_expr : CRep.t_simple_expr list :> CRep.all_expr list);
        self#newline ~post:"};" ()
      | `Vec_Assign (n,lhs,(_,rhs)) ->
        let rec assign_aux i = 
          if i = n then () else begin
            self#dump_simple_expr lhs;
            self#put_all [ "["; string_of_int i; "] = " ];
            self#dump_simple_expr rhs;
            self#put_all [ "["; string_of_int i; "];" ];
            self#newline ();
            assign_aux (succ i)
          end
        in
        assign_aux 0
    method dump_args (_,e) = self#dump_simple_expr e
  end

class static_emitter buf = 
  object(self)
    inherit Emit.emitter buf
    method dump_static name (ty,expr) = 
      self#put_i  "static ";
      self#put @@ type_to_string @@ to_monomorph_c_type [] ty;
      self#put_all [ " "; name; " = " ];
      self#dump_static_expr expr;
      self#newline ~post:";" ()
    method private dump_static_expr (expr : CRep.static_expr)  = 
      match expr with
      | `Var s -> self#put s
      | `Literal l -> self#put l
      | `Deref e -> 
        self#put "*";
        self#dump_static_expr e
      | `Address_of e ->
        self#put "&";
        self#dump_static_expr e
      | `UnOp (op,e) ->
        self#put_all [ "("; string_of_unop op; "(" ];
        self#dump_static_expr e;
        self#put "))"
      | `BinOp (op,e1,e2) ->
        self#put "((";
        self#dump_static_expr e1;
        self#put_all [ ") "; string_of_binop op; " (" ];
        self#dump_static_expr e2;
        self#put "))"
      | `Init tl ->
        self#put "{";
        self#put_many ", " self#dump_static_expr tl;
        self#put "}"
      | `Tagged_Init (tag,tl) ->
        self#put_all [ "{ ."; tag; " = { "];
        self#put_many ", " self#dump_static_expr tl;
        self#put "}"
      | `Cast (cast_ty,e) ->
        self#put "((";
        self#put @@ type_to_string @@ to_monomorph_c_type [] cast_ty;
        self#put ")";
        self#dump_static_expr e;
        self#put ")"
  end

let simple_type_repr t = 
  match t with
  | `Unit -> "char"
  | `Bool -> "uint8_t"
  | `Int (`Bit_Size w) ->
    Printf.sprintf "int%d_t" w
  | `UInt (`Bit_Size w) -> 
    Printf.sprintf "uint%d_t" w
  | `Int `Ptr_Size ->
    "int64_t"
  | `UInt `Ptr_Size ->
    "uint64_t"
  | `Float i -> if i = 32 then "float" else if i = 64 then "double" else assert false
  | `Char -> "uint32_t"

let simplified_ir = Hashtbl.create 10;;

let memo_get_simple_ir fn_name f_def = 
  if Hashtbl.mem simplified_ir fn_name then
    Hashtbl.find simplified_ir fn_name
  else begin
    let ir = CRep.get_simple_ir f_def.Ir.fn_body in
    Hashtbl.add simplified_ir fn_name ir;
    ir
  end

let simplified_static = Hashtbl.create 10;;

let memo_get_simple_static static_name = 
  if Hashtbl.mem simplified_static static_name then
    Hashtbl.find simplified_static static_name
  else begin
    let (ty,s_def) = Env.EnvMap.find Env.static_env static_name in
    let simple_def = CRep.get_simple_static s_def in
    Hashtbl.add simplified_static static_name (ty,simple_def);
    (ty,simple_def)
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
  emit_type `Char;
  emit_type (`Float 32);
  emit_type (`Float 64);
  List.iter (fun size ->
      emit_type (`UInt (`Bit_Size size));
      emit_type (`Int (`Bit_Size size))
    ) int_sizes;
  emit_type (`UInt `Ptr_Size);
  emit_type (`Int `Ptr_Size)

let emit_typedefs out_channel inst = 
  match inst with
  (* for fixed vec, there is no implementation, the implementation
   * IS a typedef
   *)
  | (`Fixed_Vec _,_) -> ()
  | _ -> 
    let ty = type_of_inst inst in
    let mangled_ty = type_to_string ty in
    let struct_name = c_struct_name @@ mangled_ty in
    Printf.fprintf out_channel "typedef struct %s %s;\n" struct_name @@ mangled_ty

let sig_of_fdef fn_def mono_args buf = 
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
  Buffer.add_string buf ")"

let emit_fn_def out_channel buf (fn_name,mono_args) = 
  if fn_name = "crust_abort" then begin
    Buffer.add_string buf @@ type_to_string `Unit;
    Buffer.add_string buf " crust_abort() { __CPROVER_assume(0); }\n";
    Buffer.output_buffer out_channel buf;
    Buffer.clear buf
    end 
  else if Intrinsics.is_intrinsic_fn fn_name then 
    ()
  else
    let fn_def = Env.EnvMap.find Env.fn_env fn_name in
    let simple_ir = memo_get_simple_ir fn_name fn_def in
    sig_of_fdef fn_def mono_args buf;
    Buffer.add_string buf " ";
    let expr_emitter = new expr_emitter buf @@ Types.type_binding fn_def.Ir.fn_tparams mono_args in
    expr_emitter#dump_expr simple_ir;
    Buffer.output_buffer out_channel buf;
    Buffer.clear buf;
    ()

let emit_fsigs out_channel f_list = 
  let buf = Buffer.create 1000 in
  List.iter (fun (f_name,m_args) ->
      let m_args = List.map c_type_of_monomorph m_args in
      if Intrinsics.is_intrinsic_fn f_name then
        let m_strings = List.map type_to_string m_args in
        let mangled_name = mangle_fn_name f_name m_args in
        Intrinsics.emit_intrinsic_inst f_name mangled_name m_strings buf;
        Buffer.add_string buf "\n"
      else
        let f_def = Env.EnvMap.find Env.fn_env f_name in
        sig_of_fdef f_def m_args buf;
        Buffer.add_string buf ";\n"
    ) f_list;
  Buffer.output_buffer out_channel buf

let emit_statics out_channel s_list = 
  let buf = Buffer.create 1000 in
  let emitter = new static_emitter buf in
  List.iter (fun static ->
      let d = memo_get_simple_static static in
      emitter#dump_static static d
    ) s_list;
  Buffer.output_buffer out_channel buf

(* at this point our types are still in terms of Refs, Ptrs, Vecs, and Strs.
   However in C there is no such distinction. Therefore a Foo<*T> and a
   Foo<&T> will compile to the precisely the same data structure. So at
   this point we eliminate type instantiations that are
   indistinguishable, and instead work at the level raw pointers.

   We also references to DSTs Vecs and Strs into their runtime representation.

   This also means throwing away some information in our instantiations.
   For instance, when performing monomorphization we track Vecs, Str, and Tuples
   as distinct instantiations because in some contexts---(API discovery),
   test generation, and so on--- this matters. But when generating C code it does not.

   However, at this point in the compilation process we don't particularly care:
   an instantiation of a Vec true,t is exactly the same as a Tuple [*mut t, u32] so
   when checking for duplicate instances we erase this information.

   Note that abstract method resolution still operates at the level of mono_types,
   i.e. we can still resolve vecs and tuples without ambiguity if there are impls
   for both.
*)

(* XXX: COPY PASTE *)
let rec uniq_list = function
  | [] -> []
  | h::h'::t when h = h' ->
    h::(uniq_list t)
  | h::t ->
    h::(uniq_list t)

let find_dup_ty_inst =
  let make_dst_inst mut wrapped = 
  if mut then
    `Tuple,[`Ptr_Mut (c_type_of_monomorph wrapped); slice_len_type]
  else
    `Tuple,[`Ptr (c_type_of_monomorph wrapped); slice_len_type]
  in
  fun insts ->
    let simple_inst = 
      List.map (fun (inst_flag,m_args) ->
          match inst_flag with
          | `String mut ->
            make_dst_inst mut char_data_type
          | `Vec mut ->
            make_dst_inst mut (List.hd m_args)
          | (`Fixed_Vec _ as n)
          | (`Tuple as n)
          | (`Adt _ as n) ->
            (n,(List.map c_type_of_monomorph m_args))
        ) insts
    in
    List.sort Pervasives.compare simple_inst
    |> uniq_list

let find_dup_fn_inst = 
  let rec simple_refs : Types.mono_type -> Types.mono_type = function
    | `Ref (_,t) -> `Ref ("_dummy_",(simple_refs t))
    | `Ref_Mut (_,t) -> `Ref_Mut ("_dummy_", (simple_refs t))
    | `Tuple tl -> `Tuple (List.map simple_refs tl)
    | `Ptr t -> `Ptr (simple_refs t)
    | `Ptr_Mut t -> `Ptr_Mut (simple_refs t)
    | #Types.simple_type as s -> s
    | `Vec t -> `Vec (simple_refs t)
    | `Str -> `Str
    | `Bottom -> `Bottom
    | `Adt_type a -> 
      `Adt_type { a with Types.type_param = List.map simple_refs a.Types.type_param }
    | `Fixed_Vec (n,t) -> 
      `Fixed_Vec (n,simple_refs t)
  in
  fun insts -> 
    let simple_inst = 
      List.map (fun (f_name,m_args) ->
          (f_name,(List.map simple_refs m_args))
        ) insts in
    List.sort Pervasives.compare simple_inst |> uniq_list


module CTypeInst = struct
  type t = c_inst_flag * (c_types list)
  let compare = Pervasives.compare
end

module CISet = Set.Make(CTypeInst)

module CIMap = Map.Make(CTypeInst)

let add_inst (accum : CISet.t) (ty : c_types) =
  match ty with
  | `Tuple tl -> CISet.add (`Tuple,tl) accum
  | `Adt_type { Types.type_name = t; Types.type_param = tl } ->
    CISet.add (`Adt t,tl) accum
  | `Fixed_Vec (n,t) ->
    CISet.add (`Fixed_Vec n,[t]) accum
  | _ -> accum

let rec build_dep_map (accum : CISet.t CIMap.t) ((inst_flag,type_args) as inst) =
  match inst_flag with
  | `Fixed_Vec _
  | `Tuple ->
    CIMap.add inst (List.fold_left add_inst CISet.empty type_args) accum
  | `Adt type_name ->
    let t_def = Env.EnvMap.find Env.adt_env type_name in
    let ref_types = 
      match t_def with
      | `Enum_def ed -> 
        let b = Types.type_binding ed.Ir.e_tparam (type_args : c_types list :> Types.mono_type list) in
        List.concat @@ List.map (fun ed ->
            List.map (to_monomorph_c_type b) ed.Ir.variant_fields
        ) ed.Ir.variants
      | `Struct_def sd ->
        let b = Types.type_binding sd.Ir.s_tparam (type_args : c_types list :> Types.mono_type list) in
        List.map (fun (_,t) -> to_monomorph_c_type b t) sd.Ir.struct_fields
    in
    CIMap.add inst (List.fold_left add_inst CISet.empty ref_types) accum

module SMap = Map.Make(String)

let build_static_deps = 
  let rec static_dep_aux accum expr = 
    match expr with
    | `Var s -> SSet.add s accum
    | `Literal l -> accum
    | `UnOp (_,s)
    | `Address_of s
    | `Deref s -> static_dep_aux accum s
    | `Tagged_Init (_,tl)
    | `Init tl ->
      List.fold_left static_dep_aux accum tl
    | `BinOp (_,e1,e2) ->
      List.fold_left static_dep_aux accum [e1;e2]
    | `Cast (_,e) -> static_dep_aux accum e
  in
  let dep_of_static static_name =
    let (_,s_def) = memo_get_simple_static static_name in
    static_dep_aux SSet.empty s_def
  in
  fun static_set ->
    SSet.fold (fun s_name accum ->
        SMap.add s_name (dep_of_static s_name) accum
      ) static_set SMap.empty

module TopoSort(M : Map.S)(S: Set.S with type elt = M.key) = struct
  let rec topo_sort dep_map accum = 
    let (dep_met,has_dep) = M.partition (fun _ deps ->
        S.is_empty deps
      ) dep_map
    in
    if M.cardinal dep_met = 0 &&
       M.cardinal has_dep = 0 then
      List.rev accum
    else if M.cardinal dep_met = 0 then
      failwith "dependency cycle!"
    else begin
      let (accum,dep_met_s) = M.fold (fun key _ (accum,dms) ->
        key::accum,S.add key dms
      ) dep_met (accum,S.empty) in
      let dep_map' = M.map (fun deps -> S.diff deps dep_met_s) has_dep in
      topo_sort dep_map' accum
    end
end

module STopo = TopoSort(SMap)(SSet)
let stopo_sort = STopo.topo_sort

module TTopo = TopoSort(CIMap)(CISet)
let topo_sort = TTopo.topo_sort

let order_types t_list =
  let d_map = List.fold_left build_dep_map CIMap.empty t_list in
  topo_sort d_map []

let includes = [
  "stdint.h";
  "stdlib.h";
  "string.h";
  "stddef.h";
  "assert.h"
]

let dump_includes out_channel = 
  List.iter (fun i ->
      Printf.fprintf out_channel "#include <%s>\n" i
    ) includes

let crust_mem_limit = ref 5;;
let gcc_mode = ref false;;

let emit out_channel t_set f_set statics = 
  let t_list = order_types @@ find_dup_ty_inst @@ Analysis.TISet.elements t_set in
  let f_list = find_dup_fn_inst @@ Analysis.FISet.elements f_set in
  emit_common_typedefs out_channel;
  begin
    if !gcc_mode then begin
      Printf.fprintf out_channel "#define assert(x)\n#define __CPROVER_assume(x) 0\n"
    end else ()
  end;
  dump_includes out_channel;
  Printf.fprintf out_channel "#define CRUST_MAX_MEM %d\n" !crust_mem_limit;
  List.iter (emit_typedefs out_channel) t_list;
  (
    if Intrinsics.need_iheader (f_list :> (string * Types.r_type list) list)  then
      Printf.fprintf out_channel "#include \"rust_intrinsics.h\"\n"
    else ()
  );
  Printf.fprintf out_channel "#include \"crust_intrinsics.h\"\n";
  let type_buffer = Buffer.create 1000 in
  let type_emitter = new typedef_emitter type_buffer in
  List.iter (fun t_inst ->
      type_emitter#emit_type_instantiation t_inst
    ) t_list;
  Buffer.output_buffer out_channel type_buffer;
  Buffer.reset type_buffer;

  let statics = stopo_sort (build_static_deps statics) [] in
  emit_statics out_channel statics;

  emit_fsigs out_channel f_list;
  let fn_buffer = Buffer.create 1000 in
  List.iter (emit_fn_def out_channel fn_buffer) f_list;
  Printf.fprintf out_channel "\n"
