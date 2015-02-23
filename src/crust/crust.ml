let compile_tests test_chunk_size output_prefix = 
  let test_slices = Analysis.run_test_analysis test_chunk_size in
  List.iteri (fun test_num test_slice ->
      let out_chan = open_out @@ Printf.sprintf "%s_%d.c" output_prefix test_num in
      Compilation.emit out_chan test_slice.Analysis.type_inst test_slice.Analysis.fn_inst test_slice.Analysis.static_var;
      close_out out_chan
    ) test_slices

let generate_driver out_chan = 
  let w_state = Analysis.run_analysis () in
  RustGen.gen_driver out_chan w_state.Analysis.public_type w_state.Analysis.public_fn

let code_gen out_chan = 
  let w_state = Analysis.run_analysis () in
  Compilation.emit out_chan 
      w_state.Analysis.type_inst
      w_state.Analysis.fn_inst
      w_state.Analysis.static_var

let do_it f = 
  Printexc.record_backtrace true;
  let input_file = ref "" in
  let driver_gen = ref false in
  let test_gen = ref false in
  let test_output_file = ref "" in
  let test_chunk_size = ref 300 in
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
       ), "Output to file");
    ("-max-memory", Arg.Set_int Compilation.crust_mem_limit, "Maximum bound of memory that can be allocated");
    ("-test-size", Arg.Set_int test_chunk_size, "Number of test cases per file");
    ("-test-case-prefix", Arg.Set_string test_output_file, "Prefix for test case files generated in -test-compile")
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
  if !test_gen then
    compile_tests !test_chunk_size !test_output_file 
  else if !driver_gen then
    generate_driver !output_channel
  else
    code_gen !output_channel
(*    
  in
  if !driver_gen then
    RustGen.gen_driver !output_channel w_state.Analysis.public_type w_state.Analysis.public_fn
  else
    Compilation.emit !output_channel 
      w_state.Analysis.type_inst
      w_state.Analysis.fn_inst
      w_state.Analysis.static_var*)

let _ = do_it Sys.argv.(1)
