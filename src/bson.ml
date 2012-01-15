open Util

module Calendar = CalendarLib.Calendar
module ES = ExStream
module S = Stream

(* TODO: make functor to use custom types for list at least *)
(* TODO: use Res monad (manatki are cool!)  *)
(* TODO: check ranges for timestamps *)

exception Bson_error of string
let bson_error fmt = Printf.ksprintf (fun s -> raise (Bson_error s)) fmt


module ObjectId = struct
  type t = string

  let of_string x = x
  and to_string x =
    if String.length x = 12 then x else bson_error "objectid"
end


module Document = struct
  include Map.Make(String)

  let keys   d = fold (fun k _ l -> k :: l) d []
  let values d = fold (fun _ v l -> v :: l) d []

  let of_list l = List.fold_right (fun (k, v) -> add k v) l empty
  let to_list   = bindings
end


type element =
  | Double of float
  | String of string
  | Document of document
  | Array of element list
  | BinaryData of binary (* change it *)
  | ObjectId of ObjectId.t
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
and binary =
  | Generic of string
  | Function of string
  | GenericOld of string
  | UUID of string
  | MD5 of string
  | UserDefined of string
and cstring  = string
and document = element Document.t

module Build = struct
  module Binary = struct
    let generic s = BinaryData (Generic s)
    and f s = BinaryData (Function s)
    and generic_old s = BinaryData (GenericOld s)
    and uuid s = BinaryData (UUID s)
    and md5 s = BinaryData (MD5 s)
    and custom s = BinaryData (UserDefined s)
  end

  let double f = Double f
  and string s = String s
  and document d = Document d
  and array l = Array l
  and objectid s = ObjectId (ObjectId.of_string s)
  and datetime f = Datetime (Calendar.from_unixfloat f)
  and null = Null
  and boolean b = Boolean b
  and regex p opts = Regex (p, opts)  (* TODO(superbobry): use Str.regexp? *)
  and jscode s = JSCode s
  and jscode_with_scope s scope = JSCodeWithScope (s, scope)
  and symbol s = Symbol s
  and int32 v = Int32 v
  and timestamp v = Timestamp v
  and int64 v = Int64 v
  and minkey = Minkey
  and maxkey = Maxkey
end


module Show = struct
  let rec document d =
    let bindings = Document.fold (fun k v acc ->
      Printf.sprintf "%S: %s" k (element v) :: acc
    ) d []

    in Printf.sprintf "{%s}" (String.concat ", " (List.rev bindings))

  and element = function
    | Double d -> Printf.sprintf "Double %f" d
    | String s -> Printf.sprintf "String %s" s
    | JSCode c -> Printf.sprintf "JSCode %s" c
    | Symbol s -> Printf.sprintf "Symbol %s" s
    | Datetime d -> CalendarLib.Printer.Calendar.to_string d
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
    | Document d -> document d
    | Array a ->
      let elements = List.map element a in
      Printf.sprintf "[%s]" (String.concat ", " elements)
    | JSCodeWithScope _ -> "<unknown>"

  and binary = function
    | Generic s -> Printf.sprintf "Generic %s" s
    | Function f -> Printf.sprintf "Function %s" f
    | GenericOld s -> Printf.sprintf "GenericOld %s" s
    | UUID u -> Printf.sprintf "UUID %s" u
    | MD5 h -> Printf.sprintf "MD5 %s" h
    | UserDefined s -> Printf.sprintf "UserDefined %s" s
end


let of_stream bytes =
  let rec parse_document st = Document.of_list (parse_list st)
  and parse_list =
    let rec inner acc = parser
      | [< ''\x00' >] -> List.rev acc
      | [< 'code; key = parse_cstring; el = parse_element code; st >] ->
        inner ((key, el) :: acc) st
      | [< >] -> bson_error "parse_list: doesn't contain null byte"
    in parser
      | [< len = parse_int32; st; >] -> inner [] (ES.take_int32 len st)
      | [< >] -> bson_error "parse_list"
  and parse_element c st = match c with
    | '\x01' -> Build.double (parse_double st)
    | '\x02' -> Build.string (parse_string st)
    | '\x03' -> Build.document (parse_document st)
    | '\x04' -> Build.array (List.map snd <| parse_list st)
    | '\x05' -> BinaryData (parse_binary st)
    | '\x07' -> ObjectId (ES.take_string 12 st)
    | '\x08' -> Boolean (parse_boolean <| S.next st)
    | '\x09' -> Build.datetime (parse_double st)
    | '\x0A' -> Build.null
    | '\x0B' -> let pattern = parse_cstring st in
                let opts = parse_cstring st in
                Regex (pattern, opts)
    | '\x0D' -> JSCode (parse_string st)
    | '\x0E' -> Build.symbol (parse_string st)
    | '\x0F' -> JSCodeWithScope (
      (s_comb (flip ES.take_int32) parse_int32 st) |> parse_jscode)
    | '\x10' -> Build.int32 (parse_int32 st)
    | '\x11' -> Timestamp (parse_int64 st)
    | '\x12' -> Build.int64 (parse_int64 st)
    | '\xFF' -> Build.minkey
    | '\x7F' -> Build.maxkey
    | c -> bson_error "parse_element: invalid type: %s" (Char.escaped c)
  and parse_cstring = ES.take_while (fun c -> c <> '\x00') >> ES.to_string
  and parse_double = ES.take_string 8 >> unpack_float
  and parse_int32 = ES.take_string 4 >> unpack_int32
  and parse_int64 = ES.take_string 8 >> unpack_int64
  and parse_boolean = function
    | '\x00' -> false
    | '\x01' -> true
    |  _ -> bson_error "parse_boolean"
  and parse_string = parser
    | [< len = parse_int32; rest >] ->
      let len' = Int32.sub len 1l in
      let int_len = Int32.to_int len' in
      let s = ES.take_int32 len' rest |> ES.to_string ~len:int_len
      in S.junk rest ; s (* junk trailing null *)
    | [< >] -> bson_error "parse_string"
  and parse_binary = parser
    | [< len = parse_int32; 'c; st >] -> ES.take_string_int32 len st |>
                                         parse_subtype c
    | [< >] -> bson_error "parse_binary"
  and parse_subtype c st = match c with
    | '\x00' -> Generic st
    | '\x01' -> Function st
    | '\x02' -> GenericOld st
    | '\x03' -> UUID st
    | '\x05' -> MD5 st
    | '\x80' -> UserDefined st
    | _ -> bson_error "invalid binary subtype!"
  and parse_jscode = parser
    | [< st = parse_string; doc = parse_document >] -> (st, doc)
    | [< >] -> bson_error "parse_jscode"
  in

  let result =
    try
      parse_document bytes
    with S.Failure ->
      bson_error "malformed bson data"
  in match S.peek bytes with
    | Some c ->
      bson_error "data after trailing null byte! %c" c
    | None   -> result

let of_string = S.of_string >> of_stream

let of_file = flip with_file_in <| S.of_channel >> of_stream

let to_buffer document =
  let buf = Buffer.create 16 in
  let addc = Buffer.add_char buf in
  let adds = Buffer.add_string buf in
  let curpos () = Buffer.length buf in
  let dummy = "\000\000\000\000" in
  let patch_length pos =
    let len = Int32.of_int & (Buffer.length buf) - pos in
    buffer_change_substring buf pos & pack_int32 len
  in
  let rec encode_document doc pos = match doc with
    | (key, element)::tail ->
      encode_element element (fun chr -> addc chr; encode_cstring key);
      encode_document tail pos
    | _ -> addc '\x00'; patch_length pos
  and encode_element el addp = match el with
    | Double d -> addp '\x01'; adds <| pack_float d
    | String s -> addp '\x02'; encode_string s
    | Document d -> let () = addp '\x03' in
                    let pos = curpos () in
                    adds dummy; encode_document (Document.to_list d) pos
    | Array l -> let len = List.length l in
                 let d = List.combine (List.map string_of_int <| range len) l in
                 let () = addp '\x04' in
                 let pos = curpos () in
                 adds dummy; encode_document d pos
    | BinaryData bd -> addp '\x05'; encode_binary bd
    | ObjectId s -> addp '\x07'; adds s
    | Boolean b -> addp '\x08'; addc (if b then '\x01' else '\x00')
    | Datetime dt -> addp '\x09'; adds & pack_float & Calendar.to_unixfloat dt
    | Null -> addp '\x0A'
    | Regex (first, sec) -> addp '\x0B'; encode_cstring first; encode_cstring sec
    | JSCode s -> addp '\x0D'; encode_string s
    | Symbol s -> addp '\x0E'; encode_string s
    | JSCodeWithScope (s, d) -> let () = addp '\x0f' in
                                let pos_js = curpos () in
                                let () = adds dummy; encode_string s in
                                let pos_d = curpos () in
                                encode_document (Document.to_list d) pos_d;
                                patch_length pos_js
    | Int32 i -> addp '\x10'; adds <| pack_int32 i
    | Timestamp l -> addp '\x11'; adds <| pack_int64 l
    | Int64 l -> addp '\x12'; adds <| pack_int64 l
    | Minkey -> addp '\xFF'
    | Maxkey -> addp '\x7F'
  and encode_string s =
    (* length with trailing null byte *)
    let len = Int32.add 1l & str_length_int32 s in
    adds & pack_int32 len; encode_cstring s
  and encode_cstring s = adds s; addc '\x00'
  and encode_binary bd =
    (* think, that i should patch length here too *)
    let (c, st) = encode_subtype bd in
    let len = str_length_int32 st in
    adds <| pack_int32 len; addc c; adds st
  and encode_subtype bd = match bd with
    | Generic st -> ('\x00', st)
    | Function st -> ('\x01', st)
    | GenericOld st -> ('\x02', st)
    | UUID st -> ('\x03', st)
    | MD5 st -> ('\x05', st)
    | UserDefined st -> ('\x80', st)
  in
  let () = adds dummy; encode_document (Document.to_list document) 0 in
  buf

let to_string = to_buffer >> Buffer.contents
