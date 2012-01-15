open Bson
open CalendarLib
open Kaputt.Abbreviations

module B = Build

module BSONGen = struct
  module G = Gen

  let map1 builder gen = G.map1 builder Show.element gen

  let cstring = G.word (G.make_int 0 0xffff)

  let objectid = map1 B.objectid (G.word (G.lift 12 "12"))
  let double = map1 B.double G.float
  let string = map1 B.string cstring
  let datetime = map1 B.datetime G.float
  let null = G.lift B.null "null"
  let boolean = map1 B.boolean G.bool
  let regex = map1 (G.apply2 B.regex) (G.zip2 cstring cstring)
  let jscode = map1 B.jscode cstring
  let symbol = map1 B.symbol cstring
  let int32 = map1 B.int32 G.int32
  let int64 = map1 B.int64 G.int64
  let timestamp = map1 B.timestamp G.int64
  let minkey = G.lift B.minkey "Minkey"
  let maxkey = G.lift B.maxkey "Maxkey"
  let binary = G.choose_list [
    map1 B.Binary.generic cstring;
    map1 B.Binary.f cstring;
    map1 B.Binary.generic_old cstring;
    map1 B.Binary.uuid cstring;
    map1 B.Binary.md5 cstring;
    map1 B.Binary.custom cstring
  ]

  (* Primitive types. *)
  let element = G.choose_list [
      objectid;
      double;
      string;
      datetime;
      null;
      boolean;
      regex;
      jscode;
      int32;
      int64;
      timestamp;
      minkey;
      maxkey;
      binary
  ]

  (* Compound types. *)
  let array = map1 B.array (G.list (G.make_int 0 0xf) element)
  let document =
    let l = G.list (G.make_int 0 0xf)
      (G.zip2 cstring (G.choose_list [element; array]))
    in G.map1 Document.of_list Show.document l

  (* TODO(superbobry): add JSCodeWithScope! *)
end


let test_parse_unparse = Test.make_random_test
  ~title:"parse<->unparse"
  BSONGen.document
  (fun d -> d)
  [Spec.always ==> (fun d1 ->
    let d2 = of_string (to_string d1) in

    try
      Document.compare Pervasives.compare d1 d2 = 0
    with
      | Bson_error s -> print_endline s; false
      | Stream.Failure -> print_endline "Stream.Failure"; false
      | Stream.Error s -> Printf.printf "Stream.Error %s\n" s; false
   )
  ]


let () = Test.run_tests [test_parse_unparse]
