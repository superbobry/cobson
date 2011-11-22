open Camlp4.PreCast
open Pa_type_conv

let raise_unsupported () =
  failwith "Unsupported use of bson (you can only use it on records)."


module Gen_struct = struct
  let record ~record_name _loc ty =
    <:str_item<
      module Fields = struct
      end
    >>
  ;;

  let mani ~record_name ty =
    match ty with
    | <:ctyp@loc< { $x$ } >> ->
      record ~record_name loc x
    | _ -> failwith "the right hand side of the manifest must be a record"

  let fields_of_ty _loc ~record_name ~tps:_ ~rhs =
    let unsupported = (fun _ _ -> raise_unsupported ()) in
    Gen.switch_tp_def
      ~alias:    unsupported
      ~sum:      unsupported
      ~variants: unsupported
      ~mani:     (fun (_:Loc.t) _tp1 tp2 -> mani ~record_name tp2)
      ~nil:      (fun _ -> raise_unsupported ())
      ~record:   (record ~record_name)
      rhs

  let generate = function
    | Ast.TyDcl (_loc, name, tps, rhs, _) -> fields_of_ty _loc ~record_name:name ~tps ~rhs
    | Ast.TyAnd (_loc, _, _) as tds ->

        ignore (_loc, tds);
        failwith "Not supported"
    | _                             -> assert false
end


let () = add_generator "bson" Gen_struct.generate
