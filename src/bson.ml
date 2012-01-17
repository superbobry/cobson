open Util

module Calendar = CalendarLib.Calendar
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


module Parse = struct
  let rec document st = Document.of_list (list st)

  and list =
    let rec inner acc = parser
      | [< ''\x00' >] -> List.rev acc
      | [< 'kind; key = cstring; value = element kind; st >] ->
        inner ((key, value) :: acc) st
      | [< >] -> bson_error "list doesn't contain null byte"
    in parser
      | [< len = int32; st; >] -> inner [] (S.take_int32 len st)
      | [< >] -> bson_error "invalid list"

  and element kind st = match kind with
    | '\x01' -> Build.double (double st)
    | '\x02' -> Build.string (string st)
    | '\x03' -> Build.document (document st)
    | '\x04' -> Build.array (List.map snd <| list st)
    | '\x05' -> BinaryData (binary st)
    | '\x07' -> ObjectId (S.take_string 12 st)
    | '\x08' -> Boolean (boolean <| S.next st)
    | '\x09' -> Build.datetime (double st)
    | '\x0A' -> Build.null
    | '\x0B' ->
      let pattern = cstring st and opts = cstring st in
      Regex (pattern, opts)
    | '\x0D' -> JSCode (string st)
    | '\x0E' -> Build.symbol (string st)
    | '\x0F' ->
      (* FIXME(Sergei): unobfuscate! *)
      JSCodeWithScope ((s_comb (flip S.take_int32) int32 st) |> jscode)
    | '\x10' -> Build.int32 (int32 st)
    | '\x11' -> Timestamp (int64 st)
    | '\x12' -> Build.int64 (int64 st)
    | '\xFF' -> Build.minkey
    | '\x7F' -> Build.maxkey
    | _      -> bson_error "invalid element kind: %C" kind

  and cstring = S.take_while ((<>) '\x00') >> S.to_string
  and int32 = S.take_string 4 >> unpack_int32
  and int64 = S.take_string 8 >> unpack_int64
  and double = S.take_string 8 >> unpack_float
  and boolean = function
    | '\x00' -> false
    | '\x01' -> true
    |  v -> bson_error "invalid boolean value: %C" v

  and string = parser
    | [< len = int32; rest >] ->
      let len = Int32.to_int len in
      let str = S.take_string (len - 1) rest in
      S.junk rest; (* Note(Sergei): junk the trailing null. *)
      str
    | [< >] -> bson_error "invalid string"

  and binary = parser
    | [< len = int32; 'kind; st >] ->
      S.take_string_int32 len st |> subtype kind
    | [< >] -> bson_error "invalid binary"

  and subtype kind data = match kind with
    | '\x00' -> Generic data
    | '\x01' -> Function data
    | '\x02' -> GenericOld data
    | '\x03' -> UUID data
    | '\x05' -> MD5 data
    | '\x80' -> UserDefined data
    | _      -> bson_error "invalid binary subtype: %C!" kind

  and jscode = parser
    | [< code = string; scope = document >] -> (code, scope)
    | [< >] -> bson_error "invalid jscode with scope"
end


module Encode = struct
  type state = { buf : Buffer.t;
                 add : [ `Char of char
                       | `String of string
                       | `Tag of (char * string)
                       | `Patch of int ] -> unit;
                 pos : unit -> int
               }

  let dummy = String.make 4 '\000'

  let rec init () =
    let buf = Buffer.create 16 in
    let add = function
      | `Char ch  -> Buffer.add_char buf ch
      | `String s -> Buffer.add_string buf s
      | `Tag (ch, key) ->
        Buffer.add_char buf ch;
        Buffer.add_string buf key;
        Buffer.add_char buf '\000';
      | `Patch pos ->
        let len = Int32.of_int & (Buffer.length buf) - pos in
        buffer_change_substring buf pos & pack_int32 len
    and pos () = Buffer.length buf in { buf; add; pos }

  and document ({ add; _ } as state) doc pos =
    List.iter (fun (key, value) -> element state key value) doc;

    add & `Char '\x00';
    add & `Patch pos

  and element ({ add; _ } as state) key = function
    | Double d ->
      add & `Tag ('\x01', key);
      add & `String (pack_float d)
    | String s ->
      add & `Tag ('\x02', key);
      string state s
    | Document d ->
      add & `Tag ('\x03', key);
      add & `String dummy;
      document state (Document.to_list d) (state.pos ())
    | Array l ->
      let len = List.length l in
      let doc = List.combine (List.map string_of_int <| range len) l in
      let ()  = add & `Tag ('\x04', key) in
      let pos = state.pos () in
      add & `String dummy;
      document state doc pos
    | BinaryData bd -> add & `Tag ('\x05', key); binary state bd
    | ObjectId s    -> add & `Tag ('\x07', key); add & `String s
    | Boolean b     ->
      add & `Tag ('\x08', key);
      add & `Char (if b then '\x01' else '\x00')
    | Datetime dt   ->
      add & `Tag ('\x09', key);
      add & `String (pack_float & Calendar.to_unixfloat dt)
    | Regex (pattern, opts) ->
      add & `Tag ('\x0B', key);
      cstring state pattern;
      cstring state opts
    | JSCode s -> add & `Tag ('\x0D', key); string state s
    | Symbol s -> add & `Tag ('\x0E', key); string state s
    | JSCodeWithScope (code, scope) ->
      let () = add & `Tag ('\x0f', key)
      and pos_code = state.pos ()
      and () = add & `String dummy; string state code
      and pos_scope = state.pos () in
      document state (Document.to_list scope) pos_scope;
      (* Note(Sergei): patch total jscode length. *)
      add & `Patch pos_code
    | Timestamp l ->
      add & `Tag ('\x11', key);
      add & `String (pack_int64 l)
    | Int32 i ->
      add & `Tag ('\x10', key);
      add & `String (pack_int32 i)
    | Int64 l ->
      add & `Tag ('\x12', key);
      add & `String (pack_int64 l)
    | Minkey  -> add & `Tag ('\xFF', key)
    | Maxkey  -> add & `Tag ('\x7F', key)
    | Null    -> add & `Tag ('\x0A', key)

  and string state s =
    let len = Int32.of_int & String.length s + 1 in
    state.add & `String (pack_int32 len);
    cstring state s

  and cstring state s =
    state.add & `String s;
    state.add & `Char '\x00'

  and binary state b =
    (* think, that i should patch length here too *)
    let (kind, data) = subtype b in
    let len = Int32.of_int & String.length data in
    state.add & `String (pack_int32 len);
    state.add & `Char kind;
    state.add & `String data

  and subtype = function
    | Generic data -> ('\x00', data)
    | Function data -> ('\x01', data)
    | GenericOld data -> ('\x02', data)
    | UUID data -> ('\x03', data)
    | MD5 data -> ('\x05', data)
    | UserDefined data -> ('\x80', data)
end


let of_stream bytes =
  let result =
    try
      Parse.document bytes
    with S.Failure ->
      bson_error "malformed bson data"
  in match S.peek bytes with
    | Some _ ->
      bson_error "data after trailing null byte! %S"
        (Stream.to_string bytes)
    | None   -> result

let of_string = S.of_string >> of_stream

let to_buffer doc =
  let open Encode in
  let ({ add; buf; _ } as state) = init () in
  add & `String dummy;
  document state (Document.to_list doc) 0;
  buf

let to_string = to_buffer >> Buffer.contents
