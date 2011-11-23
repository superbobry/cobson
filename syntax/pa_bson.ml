open Camlp4.PreCast

module Gen = Pa_type_conv.Gen

let raise_unsupported () =
  failwith "Unsupported use of bson (you can only use it on records)."


module Gen_bson_of = struct
  let bson_of_record ~type_name _loc ty =
    let fields = Ast.list_of_ctyp ty [] in
    let aux = function
      | <:ctyp@loc< $lid:name$ : string >> ->
        <:expr@loc< ($str:name$, Bson.String $lid:name$) >>
      | _ -> assert false
    in

    let loc = Ast.loc_of_ctyp ty in
    let foo = List.map aux fields in
      <:expr@loc< $foo$ >>, loc
  ;;

  (* Generate code from type definition. *)
  let bson_of_td loc ~type_name ~tps ~rhs =
    let unsupported = (fun _ _ -> raise_unsupported ()) in
    Gen.switch_tp_def
      ~alias:    unsupported
      ~sum:      unsupported
      ~variants: unsupported
      ~mani:     (fun _ _ _ -> raise_unsupported ())
      ~nil:      (fun _ -> raise_unsupported ())
      ~record:   (bson_of_record ~type_name)
      rhs

  let generate tds =
    let (binding, _loc) = match tds with
      | Ast.TyDcl (loc, name, tps, rhs, _) -> bson_of_td loc ~type_name:name ~tps ~rhs
      | Ast.TyAnd (_loc, _, _) as tds ->
          ignore (_loc, tds);
          failwith "Not supported"
      | _ -> assert false
    in

      List.fold_right (fun x l -> <:expr< [$x$ :: $l$] >>) binding <:expr< [] >>
end


let () =
  Pa_type_conv.add_generator
    "bson"
    (fun tds ->
       let loc = Ast.loc_of_ctyp tds in
       let foo = Gen_bson_of.generate tds in
         <:str_item@loc< value x = $foo$ >>
    )
