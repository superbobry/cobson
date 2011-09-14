open Kaputt.Abbreviations
open Bson


let test_objectid = Test.make_random_test
  ~title:"ObjectId"
  (Gen.string (Gen.lift 12 "12") Gen.char)
  (fun s ->  ObjectId (to_objectid s))
  [Spec.always ==> (function
    | ObjectId s -> String.length (from_objectid s) = 12
    | Double _ | String _ | Document _ | Array _ | BinaryData _
    | Datetime _ | Null | Boolean _ | Regex _ | JSCode _
    | Symbol _ | JSCodeWithScope _ | Int32 _ | Timestamp _
    | Int64 _  | Minkey | Maxkey -> true)
  ]


let () = Test.run_tests [test_objectid]
