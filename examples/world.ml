(** A simple Cobson usage example. *)

open Printf

type t = { hello : string option } with bson

let s = bson_of_t { hello = Some "world" } in

(* a) make sure we can parse the encoded structure. *)
printf "%S\n" s;
printf "%S\n" (Bson.Show.document (Bson.of_string s));

(* b) and that 'hello' is properly coerced. *)
match t_of_bson s with
  | { hello = Some hello } ->
	printf "hello = %S\n" hello;
  | _ -> printf "oops?"
