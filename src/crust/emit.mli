class type emitter_t = object 
				method private put_many : 'a.string -> ('a -> unit) -> 'a list -> unit
				method private put_all : string list -> unit
				method private put_i : string -> unit
				method private indent : unit -> unit
				method private dedent : unit -> unit
				method private newline : ?post:string -> unit -> unit
				method private put : string -> unit
				method private open_block : unit -> unit
				method private maybe_put : string option -> unit
				method private close_block : ?post:string -> unit -> unit
			  end
class emitter : Buffer.t -> emitter_t
