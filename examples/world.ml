(** A simple Cobson usage example. *)

(** MongoDB message header struct. *)
type msg_header =  { msg_length : int32
                   ; request_id : int32
                   ; response_to : int32
                   ; op_code : int32
                   } with bson

type foo = {
  foo_f1 : int32 array;
  foo_f2 : bool list;
  foo_f3 : string option;
} with bson


print_endline (bson_of_foo { foo_f1 = [| 1l; 2l; 3l |]
                           ; foo_f2 = [false]
                           ; foo_f3 = None })
