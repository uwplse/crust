val parse_channel : ?close:bool -> in_channel -> Ir.module_expr list
val parse_string : string -> Ir.module_expr list
exception Parse_failure of (string * (string list));;
exception Unexpected_stream_end of string
