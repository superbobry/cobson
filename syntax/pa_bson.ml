open Camlp4.PreCast

module Gen = Pa_type_conv.Gen

let raise_unsupported () =
  failwith "Unsupported use of bson (you can only use it on records)."


module Inspect = struct
  let field = function
    | <:ctyp< $lid:name$ : mutable $field_ty$ >>
    | <:ctyp< $lid:name$ : $field_ty$ >> -> (name, field_ty)
    | _ -> assert false

  let fields ty = List.map field (Ast.list_of_ctyp ty [])
end


module Gen_bson_of = struct
  let bson_of_record ~type_name _loc ty =
    let aux (name, field_ty) =
      (* TODO: does mutability make any difference? *)
      let bson_ty = match field_ty with
        | <:ctyp< string >> ->
          <:expr< Bson.String $lid:name$ >>
        | <:ctyp< float >>  ->
          <:expr< Bson.Double $lid:name$ >>
        | <:ctyp< int32 >>  ->
          <:expr< Bson.Int32 $lid:name$ >>
        | <:ctyp< int64 >>  ->
          <:expr< Bson.Int64 $lid:name$ >>
        | <:ctyp< bool >>   ->
          <:expr< Bson.Boolean $lid:name$ >>
        | _ -> assert false
      in <:expr< ($str:name$, $bson_ty$) >>
    in

    let fields = Inspect.fields ty in
    let fun_body = Gen.mk_expr_lst _loc (List.map aux fields)
    and fun_args = List.map
      (fun (n, _) -> <:patt< $lid:n$ = $lid:n$ >>)
      fields
    in

    <:expr< fun [ { $list:fun_args$ } -> Bson.to_string $fun_body$ ] >>
  ;;

  (* Generate code from type definition. *)
  let bson_of_td loc ~type_name ~tps ~rhs =
    let unsupported = (fun _ _ -> raise_unsupported ()) in
    let fun_body = Gen.switch_tp_def
      ~alias:    unsupported
      ~sum:      unsupported
      ~variants: unsupported
      ~mani:     (fun _ _ _ -> raise_unsupported ())
      ~nil:      (fun _ -> raise_unsupported ())
      ~record:   (bson_of_record ~type_name)
      rhs
    and fun_name = <:patt@loc< $lid:"bson_of_" ^ type_name$ >>
    in

    <:binding@loc< $fun_name$ = $fun_body$ >>

  let generate tds = match tds with
    | Ast.TyDcl (_loc, name, tps, rhs, _) ->
      bson_of_td _loc ~type_name:name ~tps ~rhs
    | Ast.TyAnd (_loc, _, _) -> raise_unsupported ()
    | _ -> assert false
end


let () =
  Pa_type_conv.add_generator
    "bson"
    (fun tds ->
       let loc = Ast.loc_of_ctyp tds in
       <:str_item@loc< value $Gen_bson_of.generate tds$ >>)
