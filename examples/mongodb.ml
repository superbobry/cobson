(** MongoDB wire protocol implementation. 

	See also: http://mongodb.org/display/DOCS/Mongo+Wire+Protocol *)

type msg_header = { msg_length  : int32 
				  ; request_id  : int32
				  ; response_to : int32
				  ; op_code     : int32 
				  } with bson

type op_insert = { header : msg_header 
				 ; flags  : int32
				 ; full_collection_name : string
				 (* FIXME: types inside modules aren't supported atm. *)
				 (* ; documents : Bson.document list *)
				 } with bson

let main () = 
  let open Printf in 
  printf "-- MongoDB example\n"
