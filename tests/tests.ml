open Bson
open CalendarLib
open Kaputt.Abbreviations


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
    | ObjectId oid -> Printf.sprintf "ObjectId %s" (from_objectid oid)
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

  let cstring = G.word (G.make_int 0 0xffff)

  let objectid = G.map1 (fun s ->
      ObjectId (to_objectid s)) Show.element (G.word (G.lift 12 "12"))
  let double = G.map1 (fun f -> Double f) Show.element G.float
  let string = G.map1 (fun s -> String s) Show.element cstring
  let datetime = G.map1 (fun f ->
      Datetime (Calendar.from_unixfloat f)) Show.element G.float
  let null = G.lift Null "null"
  let boolean = G.map1 (fun b -> Boolean b) Show.element G.bool
  let regex = G.map1 (fun p -> Regex p) Show.element (G.zip2 cstring cstring)
  let jscode = G.map1 (fun c -> JSCode c) Show.element cstring
  let symbol = G.map1 (fun s -> Symbol s) Show.element cstring
  let int32 = G.map1 (fun v -> Int32 v) Show.element G.int32
  let int64 = G.map1 (fun v -> Int64 v) Show.element G.int64
  let timestamp = G.map1 (fun v -> Timestamp v) Show.element G.int64
  let minkey = G.lift Minkey "Minkey"
  let maxkey = G.lift Maxkey "Maxkey"
  let binary = G.choose_list [
      G.map1 (fun s -> Generic s) Show.binary cstring;
      G.map1 (fun s -> Function s) Show.binary cstring;
      G.map1 (fun s -> GenericOld s) Show.binary cstring;
      G.map1 (fun s -> UUID s) Show.binary cstring;
      G.map1 (fun s -> MD5 s) Show.binary cstring;
      G.map1 (fun s -> UserDefined s) Show.binary cstring
  ]
  let binary_data = G.map1 (fun b -> BinaryData b) Show.element binary

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
      binary_data
  ]

  (* Compound types. *)
  let array = G.map1 (fun a -> Array a) Show.element
      (G.list (G.make_int 0 0xf) element)
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
      decode (encode d) = d
    with
      | MalformedBSON s -> print_endline s; false
      | Stream.Failure -> print_endline "Stream.Failure"; false
      | Stream.Error s -> Printf.printf "Stream.Error %s\n" s; false
   )
  ]


let () = Test.run_tests [test_parse_unparse]
