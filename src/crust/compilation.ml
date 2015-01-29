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
    t.Types.type_name
  else
    t.Types.type_name ^ "_" ^
    (String.concat "_" (List.map adt_type_name t.Types.type_param))
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
  c_type_of_monomorph (TypeUtil.to_monomorph (t_bindings :> (string * Types.mono_type) list) t)

let int_sizes = [ 8; 16; 32; 64 ];;


class typedef_emitter buf = 
  object (self)
    inherit Emit.emitter buf
    method private bindings = Types.type_binding
    method emit_type_instantiation (type_name,mono_args) = 
      let type_rep = adt_of_inst (type_name,mono_args) in
      let c_name = c_struct_name type_rep in
      let r_type_rep = 
        if type_name = Types.rust_tuple_name then
          (`Tuple (mono_args : c_types list :> Types.r_type list))
        else
          (`Adt_type (type_rep : c_types Types.adt_type :> Types.poly_adt_type))
      in
      self#put_i @@ "struct " ^ c_name ^ " { // " ^ (Types.pp_t r_type_rep);
      self#newline ();
      self#indent ();
      if type_name = Types.rust_tuple_name then
        self#dump_tuple mono_args
      else begin
        let t_def = Env.EnvMap.find Env.adt_env type_name in
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
        if Intrinsics.is_intrinsic_fn fn_name then
          self#handle_intrinsic fn_name mangled_fname m_args args
        else if Env.is_abstract_fn fn_name then begin
          let arg_types = List.map (fun (t,_) -> 
              ((to_monomorph_c_type t_bindings t) : c_types :> Types.mono_type)
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
          self#handle_drop fn_name m_args args
        else begin
          self#put_i mangled_fname;
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
      self#put_i "";
      let arg_string = match args with
        | [(_,a)] -> self#with_intercept ~b:arg_buf (fun () -> self#dump_simple_expr a)
        | _ -> failwith @@ "Wrong arity to drop_glue, expected 1, found " ^ (string_of_int @@ List.length args)
      in
      let drop_type = match m_args with
        | [t] -> t
        | _ -> assert false
      in
      self#drop_type drop_type arg_string;
      self#put "0"
    (* XXX: there is a lot of overlap here with the drop action synth in driver.ml *)
    method drop_type ty arg_string = 
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

let simple_type_repr t = 
  match t with
  | `Unit -> "char"
  | `Bool -> "uint8_t"
  | `Int w ->
    Printf.sprintf "int%d_t" w
  | `UInt w -> 
    Printf.sprintf "uint%d_t" w

type c_types' = c_types

module DriverEmit = Driver.DriverF(struct
    type c_types = c_types'
    let adt_type_name = adt_type_name
    let mangle_fn_name = mangle_fn_name
    let int_sizes = int_sizes
    let type_to_string = type_to_string
    let simple_type_repr = simple_type_repr
    let to_monomorph_c_type = to_monomorph_c_type
end)

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
    Buffer.add_string buf " crust_abort() { __CPROVER_assume(0); }\n"
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

(* at this point our types are still in terms of Refs, and Ptrs.
   However in C there is no such distinction. Therefore a Foo<*T> and a
   Foo<&T> will compile to the precisely the same data structure. So at
   this point we eliminate type instantiations that are
   indistinguishable, and instead work at the level raw pointers.
*)

(* XXX: COPY PASTE *)
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

module CTypeInst = struct
  type t = string * (c_types list)
  let compare = Pervasives.compare
end

module CISet = Set.Make(CTypeInst)

module CIMap = Map.Make(CTypeInst)

let add_inst accum (ty : c_types) =
  match ty with
  | `Tuple tl -> CISet.add (Types.rust_tuple_name,tl) accum
  | `Adt_type { Types.type_name = t; Types.type_param = tl } ->
    CISet.add (t,tl) accum
  | _ -> accum

let rec build_dep_map accum ((type_name,type_args) as inst)= 
  if type_name = Types.rust_tuple_name then
    CIMap.add inst (List.fold_left add_inst CISet.empty type_args) accum
  else
    let t_def = Env.EnvMap.find Env.adt_env type_name in
    let ref_types = 
      match t_def with
      | `Enum_def ed -> 
        let b = Types.type_binding ed.Ir.e_tparam type_args in
        List.concat @@ List.map (fun ed ->
            List.map (to_monomorph_c_type b) ed.Ir.variant_fields
        ) ed.Ir.variants
      | `Struct_def sd ->
        let b = Types.type_binding sd.Ir.s_tparam type_args in
        List.map (fun (_,t) -> to_monomorph_c_type b t) sd.Ir.struct_fields
    in
    CIMap.add inst (List.fold_left add_inst CISet.empty ref_types) accum

let rec topo_sort dep_map accum = 
  let (dep_met,has_dep) = CIMap.partition (fun _ deps ->
      CISet.is_empty deps
    ) dep_map
  in
  if CIMap.cardinal dep_met = 0 &&
     CIMap.cardinal has_dep = 0 then
    List.rev accum
  else if CIMap.cardinal dep_met = 0 then
    failwith "Type dependency cycle!"
  else begin
    let (accum,dep_met_s) = CIMap.fold (fun i _ (accum,dms) ->
        i::accum,CISet.add i dms
      ) dep_met (accum,CISet.empty) in
    let dep_map' = CIMap.map (fun deps -> CISet.diff deps dep_met_s) has_dep in
    topo_sort dep_map' accum
  end

let order_types t_list =
  let d_map = List.fold_left build_dep_map CIMap.empty t_list in
  topo_sort d_map []

(* TODO: config this *)
let includes = [
  "stdlib.h";
  "string.h"
]

let dump_includes out_channel = 
  List.iter (fun i ->
      Printf.fprintf out_channel "#include <%s>\n" i
    ) includes

let emit out_channel pub_type_set pub_fn_set t_set f_set = 
  let t_list = order_types @@ find_dup_inst @@ Analysis.TISet.elements t_set in
  let f_list = find_dup_inst @@ Analysis.FISet.elements f_set in
  let (pt_list : c_types list) = List.sort Pervasives.compare @@ List.map c_type_of_monomorph @@ Analysis.MTSet.elements pub_type_set in
  let pt_list = uniq_list pt_list in
  let pf_list = find_dup_inst @@ Analysis.FISet.elements pub_fn_set in
  emit_common_typedefs out_channel;
  begin
    if !Env.gcc_mode then begin
      Printf.fprintf out_channel "#define assert(x)\n#define __CPROVER_assume(x)\n";
      dump_includes out_channel
    end else ()
  end;
  List.iter (emit_typedefs out_channel) t_list;
  (
    if Intrinsics.need_iheader (f_list :> (string * Types.r_type list) list)  then
      Printf.fprintf out_channel "#include \"rust_intrinsics.h\"\n"
    else ()
  );
  let type_buffer = Buffer.create 1000 in
  let type_emitter = new typedef_emitter type_buffer in
  List.iter (fun t_inst ->
      type_emitter#emit_type_instantiation t_inst
    ) t_list;
  Buffer.output_buffer out_channel type_buffer;
  Buffer.reset type_buffer;
  emit_fsigs out_channel f_list;
  let fn_buffer = Buffer.create 1000 in
  List.iter (emit_fn_def out_channel fn_buffer) f_list;
  Buffer.clear fn_buffer;
  if Env.EnvMap.mem Env.fn_env "crust_init" then
    let driver_emit = new DriverEmit.driver_emission fn_buffer pt_list in
    driver_emit#emit_driver pf_list;
    Buffer.output_buffer out_channel fn_buffer
  else ();
  print_newline ()
