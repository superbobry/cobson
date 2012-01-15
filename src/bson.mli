(** Cobson -- BSON library for OCaml. *)

open CalendarLib

exception Bson_error of string

type cstring = string

module ObjectId : sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end

module Document : sig
  include Map.S with type key = cstring

  val keys    : 'a t -> key list
  val values  : 'a t -> 'a list
  val of_list : (key * 'a) list -> 'a t
  val to_list : 'a t -> (key * 'a) list
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
and document = element Document.t

module Build : sig
  module Binary : sig
    val generic : string -> element
    val f : string -> element
    val generic_old : string -> element
    val uuid : string -> element
    val md5 : string -> element
    val custom : string -> element
  end

  val double : float -> element
  val string : string -> element
  val document : document -> element
  val array : element list -> element
  val objectid : string -> element
  val datetime : float -> element
  val null : element
  val boolean : bool -> element
  val regex : cstring -> cstring -> element
  val jscode : string -> element
  val symbol : string -> element
  val jscode_with_scope : string -> document -> element
  val int32 : int32 -> element
  val timestamp : int64 -> element
  val int64 : int64 -> element
  val minkey : element
  val maxkey : element
end

module Show : sig
  val document : document -> string
  val element : element -> string
end

val of_stream : char Stream.t -> document

val of_file : string -> document

val of_string : string -> document

val to_buffer : document -> Buffer.t

val to_string : document -> string
