open Kaputt.Abbreviations
open Bson


let show = function
  | Double d -> Printf.sprintf "Double %f" d
  | Int64 _ | Timestamp _ | Int32 _ | JSCodeWithScope _
  | Symbol _ | JSCode _| Regex _ | Boolean _ | Datetime _
  | ObjectId _ | BinaryData _ | Array _ | Document _ | String _
  | Maxkey | Minkey | Null -> "<unknown>"


module Gen = struct
  include Gen

  (* a fairly small size constraint for our "random" objects. *)
  let size = make_int 1 512

  let objectid = string (lift 12 "12") char
  let cstring = string size char
  let double = map1 (fun f -> Double f) show float

  let element = choose_list [
    double;
  ]

  let document = list size (zip2 cstring element)
end


let test_objectid = Test.make_random_test
  ~title:"ObjectId"
  Gen.objectid
  (fun s -> ObjectId (to_objectid s))
  [Spec.always ==> (function
    | ObjectId s -> String.length (from_objectid s) = 12
    | Double _ | String _ | Document _ | Array _ | BinaryData _
    | Datetime _ | Null | Boolean _ | Regex _ | JSCode _
    | Symbol _ | JSCodeWithScope _ | Int32 _ | Timestamp _
    | Int64 _  | Minkey | Maxkey -> true)
  ]


let test_parseunparse = Test.make_random_test
  ~title:"parse<->unparse"
  Gen.document
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

let () = Test.run_tests [test_objectid; test_parseunparse]
