val parse_channel : in_channel -> Ir.module_expr list
val parse_string : string -> Ir.module_expr list
exception Parse_failure of (string * (string list));;
exception Unexpected_stream_end of string
(*
val consume_to_end :
           'a list ->
           ('a list -> ('b -> 'a list -> 'c) -> 'c) -> ('b list -> 'c) -> 'c
val parse_n : (string list -> ('a -> string list -> 'b) -> 'b) ->
			  string list -> ('a list -> string list -> 'b) -> 'b
val parse_module : string list ->
				   (Ir.module_expr -> string list -> Ir.module_expr list) ->
				   Ir.module_expr list
val do_test : unit -> Ir.module_expr list
 *)
