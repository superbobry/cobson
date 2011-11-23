(** A simple Cobson usage example. *)

open Bson

type t = {
  s : string;
  mutable s_m : string;
  f : float;
  mutable f_m : float;
} with bson
