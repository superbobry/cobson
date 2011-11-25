open Bson
open CalendarLib
open Kaputt.Abbreviations

module B = Build


module Show = struct
  let rec element = function
    | Double d -> Printf.sprintf "Double %f" d
    | String s -> Printf.sprintf "String %s" s
    | JSCode c -> Printf.sprintf "JSCode %s" c
    | Symbol s -> Printf.sprintf "Symbol %s" s
    | Datetime d -> Printer.Calendar.to_string d
    | Null -> "Null"
    | Minkey -> "Minkey"
    | Maxkey -> "Maxkey"
    | Boolean b -> Printf.sprintf "Boolean %b" b
    | Regex (f, s) -> Printf.sprintf "Regex %s %s" f s
    | Int32 v -> Printf.sprintf "Int32 %ld" v
    | Int64 v -> Printf.sprintf "Int64 %Ld" v
    | Timestamp v -> Printf.sprintf "Timestamp %Ld" v
    | BinaryData b -> Printf.sprintf "BinaryData %s" (binary b)
    | ObjectId oid -> Printf.sprintf "ObjectId %s" (ObjectId.to_string oid)
    | Array _
    | JSCodeWithScope _
    | Document _ -> "<unknown>"

  and binary = function
    | Generic s -> Printf.sprintf "Generic %s" s
    | Function f -> Printf.sprintf "Function %s" f
    | GenericOld s -> Printf.sprintf "GenericOld %s" s
    | UUID u -> Printf.sprintf "UUID %s" u
    | MD5 h -> Printf.sprintf "MD5 %s" h
    | UserDefined s -> Printf.sprintf "UserDefined %s" s
end


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
  let document = G.list (G.make_int 0 0xf)
      (G.zip2 cstring (G.choose_list [element; array]))

  (* TODO(superbobry): add JSCodeWithScope! *)
end


let test_parse_unparse = Test.make_random_test
  ~title:"parse<->unparse"
  BSONGen.document
  (fun d -> d)
  [Spec.always ==> (fun d ->
    try
      of_string (to_string d) = d
    with
      | Bson_error s -> print_endline s; false
      | Stream.Failure -> print_endline "Stream.Failure"; false
      | Stream.Error s -> Printf.printf "Stream.Error %s\n" s; false
   )
  ]


let () = Test.run_tests [test_parse_unparse]
