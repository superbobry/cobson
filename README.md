cobson
======

`cobson` is a simple `BSON` library for OCaml.


Example
-------

```ocaml
open Printf

type t = { hello : string } with bson

let s = bson_of_t { hello = "world" } in
printf "%S\n" s;
printf "%S\n" (Bson.Show.document (Bson.of_string s))
```
