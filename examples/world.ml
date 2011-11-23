(** A simple Cobson usage example. *)

type t = {
  foo : string;
  bar : float;
  egg : int32;
  ham : int64;
  boo : bool
} with bson
