(** A simple Cobson usage example. *)

open Bson

type t = {
    hello : string
} with bson
