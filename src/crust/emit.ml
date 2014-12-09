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
class emitter buf =
object(self)
  val mutable indent_level = 0
  val mutable indent_string = ""
  val mutable indented = false
  method private put_many : 'a.string -> ('a -> unit) -> 'a list -> unit =
	let rec put_loop first separator put_fn items = 
	  match items with
	  | [] -> ()
	  | h::t ->
		 (if not first then self#put_i separator else ());
		 put_fn h;
		 put_loop false separator put_fn t
	in
	fun sep pf i -> put_loop true sep pf i
  method private put_i s = 
	if not indented then
	  (self#put indent_string;
	   indented <- true
	  )
	else ();
	self#put s
  method private indent () = 
	indent_level <- indent_level + 1;
	indent_string <- indent_string ^ "    "
  method private dedent () = 
	indent_level <- indent_level - 1;
	indent_string <- String.sub indent_string 0 @@ (String.length indent_string) - 4
  method private newline ?post () = 
	self#maybe_put post;
	self#put "\n";
	indented <- false
  method private put s =
	Buffer.add_string buf s
  method private open_block () = 
	self#put_i "{";
	self#indent ();
	self#newline ()
  method private maybe_put : string option -> unit = function 
	| Some s -> self#put_i s
	| None -> ()
  method private close_block ?post () = 
	self#dedent ();
	self#put_i "}";
	self#maybe_put post
  method private put_all l = 
	self#put_i @@ List.hd l;
	List.iter self#put @@ List.tl l
end
