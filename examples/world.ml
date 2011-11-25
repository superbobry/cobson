(** A simple Cobson usage example. *)

type foo = {
  foo_f1 : string;
  foo_f2 : bool list;
  foo_f3 : string option;
}
and bar = {
  bar_f1 : int32
} with bson


print_endline (bson_of_foo { foo_f1 = "foo_f1"
                           ; foo_f2 = [false]
                           ; foo_f3 = None });
print_endline (bson_of_bar { bar_f1 = 0l })
