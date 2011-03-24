open CalendarLib

open Util

(* Thanks to: kiyoto (caml-mongo) *)


(* TODO: csting shouldn't contain null byte -- implement it *)
(* TODO: objectid must be 12 bytes long -- implement it *)
(* TODO: make functor to use custom types for list at least *)
(* TODO: use Res monad (manatki are cool!)  *)
(* TODO: parse_double, binary pack/unpack -- use stream *)

exception MalformedBSON of string
let malformed s = raise (MalformedBSON s)

type element =
  | Double of float
  | String of string
  | Document of document
  | Array of array
  | BinaryData of binary (* change it *)
  | Objectid of objectid
  | Datetime of Calendar.t
  | Null
  | Boolean of bool
  | Regex of (cstring * cstring)
  | JSCode of string
  | Symbol of string
  | JSCodeWithScope of (string * document)
  | Int32 of int32
  | Timestamp of int64
  | Int64 of int64
  | Minkey
  | Maxkey
and cstring = UTF8.t
and string = UTF8.t
and objectid = UTF8.t
and binary =
  | Generic of string
  | Function of string
  | GenericOld of string
  | UUID of string
  | MD5 of string
  | UserDefined of string
and document = (cstring * value) list
and array = value list (* array instead of list? *)

module S = Stream

let decode_stream bytes =
  let rec parse_document = parser
    | [< len = parse_int32; st; ''\x00' >] -> parse_list [] (S.take len st)
  and parse_list acc = parser
    | [< 'code; key = parse_cstring; el = parse_element code; st >] ->
      parse_list ((key, el)::acc) st
    | [< >] -> acc
  and parse_cstring = S.take_while (fun c -> c <> '\x00') >> S.to_string
  and parse_element c st = match c with
    | '\x01' -> Double (parse_double st)
    | '\x02' -> String (parse_string st)
    | '\x03' -> Document (parse_document st)
    | '\x04' -> Array (List.map snd & parse_document st)
    | '\x05' -> BinaryData (parse_binary st)
    | '\x07' -> Objectid (S.take_stream 12 st)
    | '\x08' -> Boolean (parse_boolean & S.next st)
    | '\x09' -> Datetime (Calendar.from_unixfloat & parse_double st)
    | '\x0A' -> Null
    | '\x0B' -> let first = parse_cstring st in
                let sec = parse_cstring st in
                Regex (first, sec)
    | '\x0D' -> JSCode (parse_string st)
    | '\x0E' -> Symbol (parse_string st)
    | '\x0F' -> JSCodeWithScope (parse_jscode st)
    | '\x10' -> Int32 (parse_int32 st)
    | '\x11' -> Timestamp (parse_int64 st)
    | '\x12' -> Int64 (parse_int64 st)
    | '\xFF' -> Minkey
    | '\x7F' -> Maxkey
    | _ -> malformed "parse_element"
  and parse_string = parser
    | [< len = parse_int32; rest >] -> let st = parse_cstring rest in
                                       if String.length_int32 st = len - 1
                                       then st
                                       else malformed "parse_string"
    | [< >] -> malformed "parse_string"
  (* we use String.length insted if UTF8.length, 'cause  *)
  (* *len* shows number of bytes, not symbols.  *)
  (* we substracts 1 from *len*, because it counts trailing null byte *)
  and parse_double = S.take_string 8 >> unpack_float
  and parse_int32 = S.take_string 4 >> unpack_int32
  and parse_int64 = S.take_string 8 >> unpack_int64
  and parse_boolean = function
    | '\x00' -> false
    | '\x01' -> true
    |  _ -> malformed "parse_boolean"
  and parse_jscode = parser
    | [< len = parse_int32; st = parse_string; doc = parse_list [] >] ->
      if List.length_int32 doc = len
      then (st, doc)
      else malformed "parse_jscode"
    | [< >] -> malformed "parse_jscode"
  and parse_binary = parser
    | [< len = parse_int32; 'c; st >] -> parse_subtype c & S.take_string len st
  and parse_subtype c st = match c with
    | '\x00' -> Generic st
    | '\x01' -> Function st
    | '\x02' -> GenericOld st
    | '\x03' -> UUID st
    | '\x05' -> MD5 st
    | '\x80' -> UserDefined st
  in try
       parse_document bytes
    with S.Failure -> malformed "malformed bson data"
