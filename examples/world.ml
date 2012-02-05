(** A simple Cobson usage example. *)

type t = { hello : string option } with bson

let main () = 
	let s = Bson.to_string (bson_of_t { hello = Some "world" }) in
	let open Printf in 

	printf "-- Hello world example\n";

	(* a) make sure we can parse the encoded structure. *)
	printf "%S\n" s;
	printf "%S\n" (Bson.Show.document (Bson.of_string s));

	(* b) and that 'hello' is properly coerced. *)
	match t_of_bson (Bson.of_string s) with
	  | { hello = Some hello } ->
		printf "hello = %S\n" hello;
	  | _ -> printf "oops?"
