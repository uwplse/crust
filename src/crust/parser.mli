val parse_channel' : (string list -> ('a -> string list -> 'a list) -> 'a list) ->
    ?close:bool -> in_channel -> 'a list
val parse_string' : (string list -> ('a -> string list -> 'a list) -> 'a list) ->
    string -> 'a list
val parse_channel : ?close:bool -> in_channel -> Ir.module_expr list
val parse_string : string -> Ir.module_expr list

val parse_expr : string list -> (Ir.expr -> string list -> Ir.expr list) -> Ir.expr list
val parse_module : string list -> (Ir.module_expr -> string list -> Ir.module_expr list) -> Ir.module_expr list

exception Parse_failure of (string * (string list));;
exception Unexpected_stream_end of string
