(** A simple Cobson usage example. *)

open Printf

type t = { hello : string
		 ; world : int32 list
		 } with bson

let s = bson_of_t { hello = "world"; world = [0l] } in
printf "%S\n" s;
printf "%S\n" (Bson.Show.document (Bson.of_string s));
printf "hello = %S\n" ((t_of_bson s).hello);
printf "world = %li\n" (List.hd (t_of_bson s).world)
