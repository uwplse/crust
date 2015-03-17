let dump_used_items out_channel t_set f_set static_set = 
  let tname_set = Analysis.TISet.fold (fun t_inst accum ->
      match t_inst with 
      | `Adt s,_ -> 
        SSet.add s accum
      | _ -> accum
    ) t_set SSet.empty in
  let fname_set = Analysis.FISet.fold (fun (f_name,_) accum ->
      SSet.add f_name accum
    ) f_set SSet.empty in
  let abstract_fn = SSet.fold (fun f_name accum ->
      try 
        let fn_def = Env.EnvMap.find Env.fn_env f_name in
        match fn_def.Ir.fn_impl with
        | None -> accum
        | Some { Ir.abstract_name = a_name; _ } -> 
          SSet.add a_name accum
      with Env.Missing_binding _ -> accum
    ) fname_set SSet.empty in
  let dump_sset = SSet.iter (fun s -> 
      output_string out_channel s;
      output_string out_channel "\n"
    ) in
  dump_sset fname_set;
  dump_sset tname_set;
  dump_sset static_set;
  dump_sset abstract_fn



let crust_action ?(f=Analysis.run_analysis) e out_channel = 
  let w_state = f () in
  e out_channel w_state.Analysis.type_inst w_state.Analysis.fn_inst w_state.Analysis.static_var

let compile_tests = crust_action ~f:Analysis.run_test_analysis Compilation.emit

let do_item_dump = crust_action dump_used_items

let code_gen = crust_action Compilation.emit

let generate_driver test_prefix test_slice = 
  let w_state = Analysis.run_analysis () in
  RustGen.gen_driver test_prefix test_slice w_state.Analysis.public_type w_state.Analysis.public_fn

let do_api_dump out_chan = 
  let w_state = Analysis.run_analysis () in
  RustGen.dump_api out_chan w_state.Analysis.public_type w_state.Analysis.public_fn

type crust_command = [
  | `Driver_Gen
  | `Test_Compile
  | `Dump_Api
  | `Dump_Used_Items
]

let do_it f = 
  Printexc.record_backtrace true;
  let test_output_file = ref "" in
  let test_chunk_size = ref 300 in
  let output_channel = ref stdout in
  let command = ref None in
  let set_command cmd = 
    match !command with
    | None -> command := Some cmd
    | _ -> raise @@ Arg.Bad "Multiple commands encountered!"
  in
  let mk_set cmd = 
    Arg.Unit (fun () -> set_command cmd)
  in
  let input_channel = ref stdin in
  let close_in = ref false in
  let arg_spec = [
    ("-gcc", Arg.Set Compilation.gcc_mode, "Turn on gcc mode");
    ("-infer-filter", Arg.String (fun f_name -> Analysis.init_type_filter f_name), "Read a list of types for which to filter public function inference from file f (one per line)");
    ("-", Arg.Unit (fun () -> 
         (* do nothing this is now the default *)
         () 
       ), "Read from stdin (deprecated)");
    ("-optional-init", Arg.Set Env.init_opt, "Make crust_init optional (for testing purposes only!)");
    ("-api-filter", Arg.String (fun f_name -> Analysis.init_fn_filter f_name), "Read a list of glob patterns");
    ("-driver-gen", mk_set `Driver_Gen, "Generate test driver");
    ("-set-api-filter", Arg.String (fun filter -> 
         Analysis.set_fn_filter filter
       ), "Set a literal glob pattern");
    ("-test-compile", mk_set `Test_Compile, "Generate C test cases");
    ("-mut-length", Arg.Set_int RustGen.mut_action_len, "Generate up to n mutative calls");
    ("-immut-length", Arg.Set_int RustGen.immut_action_len, "Generate up to n immutable calls");
    ("-no-assume-ident", Arg.Set RustGen.assume_ident_init, "Do not assume that values returned from crust_init are interchangeable");
    ("-o", Arg.String (fun f_name ->
         output_channel := open_out f_name
       ), "Output to file");
    ("-max-memory", Arg.Set_int Compilation.crust_mem_limit, "Maximum bound of memory that can be allocated");
    ("-test-size", Arg.Set_int test_chunk_size, "Number of test cases per file");
    ("-test-case-prefix", Arg.Set_string test_output_file, "Prefix for test case files generated in -driver-gen");
    ("-dump-api", mk_set `Dump_Api, "Show the inferred public api and quit");
    ("-dump-items", mk_set `Dump_Used_Items, "Dump the discovered referenced types and functions");
    ("-dump-heuristic", Arg.Unit (fun () -> 
         set_command `Driver_Gen;
         RustGen.infer_api_only := true;
         ()
       ), "Show the inferred mutable/immutable API after relevance analysis");
    (* heuristic tuning *)
    ("-no-symm-break", Arg.Set RustGen.skip_symm_break, "Disable symmetry breaking");
    ("-no-interfere-check", Arg.Set RustGen.skip_interfere_check, "Disable interference checking");
    ("-no-interest-filter", Arg.Set RustGen.skip_interesting_check, "Disable interesting-ness heuristic");
    ("-no-mut-analysis", Arg.Set RustGen.no_mut_analysis, "Do not run mutation analysis, assume all methods are mutative");
    ("-no-copy-check", Arg.Set RustGen.skip_copy_use, "Do not require copied values to be used in the test sequence")
  ] in
  Arg.parse arg_spec (fun s -> 
      try 
        input_channel := open_in s;
        close_in := true
      with Sys_error _ -> raise @@ Arg.Bad ("Failed to open input file: " ^ s)
    ) "Compile Rust IR to C";
  let ast =
    try
	  Parser.parse_channel ~close:!close_in !input_channel
    with Parser.Parse_failure (f_name,tokens) ->
	  failwith @@ "Parse failure in " ^ f_name ^ " on input " ^ (String.concat " " tokens)
  in
  Env.set_env ast;
  match !command with
  | None -> code_gen !output_channel
  | Some `Dump_Api -> do_api_dump !output_channel
  | Some `Dump_Used_Items -> do_item_dump !output_channel
  | Some `Test_Compile -> compile_tests !output_channel
  | Some `Driver_Gen -> generate_driver !test_output_file !test_chunk_size

let _ = do_it Sys.argv.(1)
