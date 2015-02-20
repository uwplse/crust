let do_it f = 
  Printexc.record_backtrace true;
  let input_file = ref "" in
  let driver_gen = ref false in
  let test_gen = ref false in
  let output_channel = ref stdout in
  let arg_spec = [
    ("-gcc", Arg.Set Env.gcc_mode, "Turn on gcc mode");
    ("-infer-filter", Arg.String (fun f_name -> Env.init_inference_filter f_name), "Read a list of types for which to filter public function inference from file f (one per line)");
    ("-", Arg.Unit (fun () -> input_file := "-"), "Read from stdin");
    ("-optional-init", Arg.Set Env.init_opt, "Make crust_init optional (for testing purposes only!)");
    ("-api-filter", Arg.String (fun f_name -> Analysis.init_fn_filter f_name), "Read a list of glob patterns");
    ("-driver-gen", Arg.Set driver_gen, "Generate test driver");
    ("-set-api-filter", Arg.String (fun filter -> 
         Analysis.set_fn_filter filter
       ), "Set a literal glob pattern");
    ("-test-compile", Arg.Set test_gen, "Generate C test cases");
    ("-mut-length", Arg.Set_int RustGen.mut_action_len, "Generate up to n mutative calls");
    ("-immut-length", Arg.Set_int RustGen.immut_action_len, "Generate up to n immutable calls");
    ("-no-assume-ident", Arg.Set RustGen.assume_ident_init, "Do not assume that values returned from crust_init are interchangeable");
    ("-no-mut-analysis", Arg.Set RustGen.no_mut_analysis, "Do not run mutation analysis, assume all methods are mutative");
    ("-o", Arg.String (fun f_name ->
         output_channel := open_out f_name
       ), "Output to file")
  ] in
  Arg.parse arg_spec (fun s -> input_file := s) "Compile Rust IR to C";
  let (input,close) = 
	if !input_file = "-" then
	  stdin,false
	else
	  (open_in !input_file),true
  in
  let ast =
    try
	  Parser.parse_channel ~close:close input 
    with Parser.Parse_failure (f_name,tokens) ->
	  failwith @@ "Parse failure in " ^ f_name ^ " on input " ^ (String.concat " " tokens)
  in
  Env.set_env ast;
  let w_state = 
    if !test_gen then
      Analysis.run_test_analysis () 
    else
      Analysis.run_analysis ()
  in
  if !driver_gen then
    RustGen.gen_driver !output_channel w_state.Analysis.public_type w_state.Analysis.public_fn
  else
    Compilation.emit !output_channel 
      w_state.Analysis.type_inst
      w_state.Analysis.fn_inst
      w_state.Analysis.static_var

let _ = do_it Sys.argv.(1)
