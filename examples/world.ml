(** A simple Cobson usage example. *)

type t = {
  foo : string;
  bar : float;
  egg : int32;
  ham : int64;
  boo : bool;
  bak : Bson.element
} with bson


print_endline (bson_of_t { foo = "foo"
                         ; bar = 42.0
                         ; egg = 42l
                         ; ham = 42L
                         ; boo = false
                         ; bak = Bson.String "bak" })
