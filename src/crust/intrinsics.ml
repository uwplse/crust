type i_def = 
  | Template of string
  | Inline of string
  | Macro of string
  | Nop

type instrinsic = {
  i_name: string;
  i_body : i_def;
  i_params : string list
}

(*
   How to add to this list:
   you need to provide three items:
   * the mangled name of the intrinsic, e.g. core_instrinsics_whatever
   * the name of the type parameters for the intrinsic (used in token replacement)
   * the implementation of the intrinsic

   The implementation can be one of four things:
   * Nop: does nothing, the calling expression is compiled to an empty string
   * Inline: replace the call with an arbitrary expression, with token replacement.
   The replacement string is the variant value
   * Template: replace the call with a call to a template function, the body of which
   is the variant value
   * Macro: do not replace the call, instantiate the function with a macro

   Token replacement:
   The template string has the following tokens replaced:
   * {t_name1}, {t_name2}, ... where t_namei is the ith type parameter name
   declared for the intrinsic. The replacement string is the string representation of the
   type's C name
   * (Inline only) {arg1}, {arg2}, ... one for each argument to the intrinsic call.
   The string is the C representation of the expression for the call to the argument (parens recommended!)
   * (Template only) {mname} a mangled name for the intrinsic, guaranteed 
   to be unique for the type parameters given

   Inline vs. Template:
   Inline the call expression at the callsite with the given template string. Template replaces the
   call with a call to an appropriately mangled function name. In a separate pass, crust
   will use the template string to generate the appropriate definiton for the function.

   Macros:
   The macros referenced in Macro constructor must be defined in the rust_intrinsics.h
   found in the directory above this. Further, cbmc must be invoked with the correct include
   flag if any macros are used. Further, macros cannot be used in conjunction with
   polymorphism.
*)

(* 0th element: intrinsic name, 1st element: macro suffix *)
let a_ops = [ ("sub", "SUB"); ("mul", "MUL"); ("add", "ADD") ];;
(* yeeeeep... *)
let arith_intrinsics = List.flatten
  @@ List.flatten
  @@ List.map (fun size ->
      List.map (fun (i_name, macro_name) ->
          List.map (fun (i_pref, macro_prefix) ->
              let f_name = Printf.sprintf "core_intrinsics_%s%d_%s_with_overflow" i_pref size i_name in
              {
                i_name = f_name;
                i_params = [];
                i_body = Macro (Printf.sprintf "%s_%s(%d)" macro_prefix macro_name size)
              }
            ) [ ("u", "UNSIGNED"); ("i", "SIGNED") ]
        ) a_ops
    ) [ 8; 16; 32; 64 ]

let i_list = arith_intrinsics @ [
  {
    i_name = "core_intrinsics_set_memory";
    i_params = [ "t1" ];
    i_body = Inline "memset({arg1}, {arg2}, sizeof({t1}) * {arg3})"
  };
  {
    i_name = "core_intrinsics_transmute";
    i_params = [ "t1"; "u1" ];
    i_body = Inline "(({u1}){arg1})"
  };
  {
    i_name = "core_intrinsics_size_of";
    i_params = [ "t1" ];
    i_body = Inline "(sizeof({t1}))"
  };
  {
    i_name = "core_intrinsics_uninit";
    i_params = ["t1"];
    i_body = Template "{t1} {mname}() { {t1} to_ret; return to_ret; }"
  };
  {
    i_name = "core_intrinsics_offset";
    i_params = ["t1"];
    i_body = Inline "(({t1}*)(((size_t){arg1}) + {arg2}))"
  };
  {
    i_name = "core_intrinsics_move_val_init";
    i_params = ["t1"];
    i_body = Inline "memcpy({arg1}, &{arg2}, sizeof({t1}))"
  };
  {
    i_name = "core_intrinsics_init";
    i_params = ["t1"];
    i_body = Template "{t1} {mname}() { {t1} to_ret; memset(&to_ret, 0, sizeof({t1})); return to_ret; }"
  };
  {
    i_name = "core_intrinsics_forget";
    i_params = [""];
    i_body = Nop
  };
  {
    i_name = "core_intrinsics_copy_memory";
    i_params = [ "t1" ];
    i_body = Inline "memmove({arg1}, {arg2}, {arg3} * sizeof({t1}))";
  };
  {
    i_name = "core_intrinsics_copy_nonoverlapping_memory";
    i_params = [ "t1" ];
    i_body = Inline "memcpy({arg1}, {arg2}, {arg3} * sizeof({t1}))"
  };
  {
    i_name = "alloc_heap_allocate";
    i_params = [];
    i_body = Inline "((char*)malloc({arg1}))"
  };
  {
    i_name = "alloc_heap_deallocate";
    i_params = [];
    i_body = Inline "free({arg1})"
  };
  {
    i_name = "alloc_heap_reallocate";
    i_params = [];
    i_body = Inline "((char*)realloc({arg1}, {arg3}))"
  };
  (* should this be 1? maybe... I don't think cbmc can really support this anyway,
     and alignment doesn't/shouldn't matter in symbolically executed code
  *)
  {
    i_name = "core_intrinsics_pref_align_of";
    i_params = [ "_" ];
    i_body = Inline "((size_t)1)"
  };
  { i_name = "core_intrinsics_min_align_of";
    i_params = [ "_" ];
    i_body = Inline "((size_t)1)"
  };
  {
    i_name = "core_intrinsics_bswap16";
    i_params = [ "_" ];
    i_body = Inline "((uint16_t)__builtin_bswap16((int16_t){arg1}, (int16_t){arg2}))"
  };
  {
    i_name = "core_intrinsics_bswap32";
    i_params = [ "_" ];
    i_body = Inline "((uint32_t)__builtin_bswap32((int32_t){arg1}, (int32_t){arg2}))"
  };
  {
    i_name = "core_intrinsics_bswap64";
    i_params = [ "_" ];
    i_body = Inline "((uint64_t)__builtin_bswap64((int64_t){arg1}, (int64_t){arg2}))"
  };
  {
    i_name = "core_intrinsics_powif32";
    i_params = [ "_" ];
    i_body = Inline "(__builtin_powfi({arg1}, {arg2}))"
  };
  {
    i_name = "core_intrinsics_powif64";
    i_params = [ "_" ];
    i_body = Inline "(__builtin_powi({arg1}, {arg2}))"
  };
  {
    i_name = "core_ops_f32__Rem_rem_fmodf";
    i_params = [ "_" ];
    i_body = Inline "(fmodf({arg1}, {arg2}))"
  };
  {
    i_name = "core_ops_f64__Rem_rem_fmod";
    i_params = [ "_" ];
    i_body = Inline "(fmod({arg1}, {arg2}))"
  };
]

let intrinsic_hash = Hashtbl.create 10;;

let intrinsic_fn = 
  List.iter (fun ({ i_name = n; _ } as i) ->
      Hashtbl.add intrinsic_hash n i;
    ) i_list

let is_intrinsic_fn x = 
  Hashtbl.mem intrinsic_hash x
let is_intrinsic_inst (x,_) = is_intrinsic_fn x

let build_binding b_names b_repl = 
  let b_names' = List.map (fun s -> "{" ^ s ^ "}") b_names in
  List.combine b_names' b_repl

let do_replacement bindings str = 
  List.fold_left (fun accum (token,repl) ->
      Str.global_replace (Str.regexp token) repl accum
    ) str bindings

let emit_intrinsic_inst i_name mangled_name t_params buf =
  let i_def = Hashtbl.find intrinsic_hash i_name in
  match i_def.i_body with 
  | Nop | Inline _ -> ()
  | Macro s -> Buffer.add_string buf s; Buffer.add_string buf "\n"
  | Template temp -> 
    Buffer.add_string buf @@ do_replacement (("{mname}",mangled_name)::(build_binding i_def.i_params t_params)) temp;
    Buffer.add_string buf "\n"

let emit_intrinsic_call i_name mangled_name t_list arg_list buf = 
  let i_def = Hashtbl.find intrinsic_hash i_name in
  match i_def.i_body with
  | Nop -> ()
  | Macro _ 
  | Template _ -> 
    begin
      Buffer.add_string buf mangled_name;
      Buffer.add_string buf "(";
      Buffer.add_string buf @@ String.concat ", " arg_list;
      Buffer.add_string buf ")";
    end
  | Inline templ ->
    let i = ref 1 in
    let arg_bindings = List.map (fun arg_expr ->
        let arg_ind = string_of_int @@ !i in
        incr i;
        let arg_token = "{arg" ^ arg_ind ^ "}" in
        (arg_token, arg_expr)
      ) arg_list in
    let r_bindings = arg_bindings @ build_binding i_def.i_params t_list in
    Buffer.add_string buf @@ do_replacement r_bindings templ


let need_iheader = List.exists (fun (name,_) ->
    if Hashtbl.mem intrinsic_hash name then
      let i_def = Hashtbl.find intrinsic_hash name in
      match i_def.i_body with | Macro _ -> true | _ -> false
    else
      false
  )
