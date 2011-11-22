(** A simple Cobson usage example. *)

open Bson
open Pa_bson

type t = {
    hello : string;
} with bson;;
