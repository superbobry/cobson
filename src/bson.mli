(** Cobson -- BSON library for OCaml. *)

exception MalformedBSON of string


module ObjectId : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end


type element =
  | Double of float
  | String of string
  | Document of document
  | Array of array
  | BinaryData of binary (* change it *)
  | ObjectId of ObjectId.t
  | Datetime of CalendarLib.Calendar.t
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
and document = (cstring * element) list
and array = element list (* array instead of list? *)
and cstring = string

val of_stream : char Stream.t -> document

val of_file : string -> document

val of_string : string -> document

val to_buffer : document -> Buffer.t

val to_string : document -> string
