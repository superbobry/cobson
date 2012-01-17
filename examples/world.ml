(** A simple Cobson usage example. *)

open Printf

type t = { hello : string } with bson

let s = bson_of_t { hello = "world" } in
printf "%S\n" s;
printf "%S\n" (Bson.Show.document (Bson.of_string s))
