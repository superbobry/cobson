open StdLabels
open Camlp4.PreCast

module Gen = Pa_type_conv.Gen

let raise_unsupported () =
  failwith "Unsupported use of bson (you can only use it on records)."


module Inspect = struct
  let field = function
    | <:ctyp@loc< $lid:name$ : mutable $field_ty$ >>
    | <:ctyp@loc< $lid:name$ : $field_ty$ >> -> (name, field_ty)
    | _ -> assert false

  let fields ty = List.map (Ast.list_of_ctyp ty []) ~f:field
end


module Gen_of_bson = struct
  let mk_type_of_bson loc patt =
    let unexpected = <:match_case@loc< e -> 
      Bson.bson_error "Unexpected BSON encountered: %s" (Bson.Show.element e) >>
    in

    <:expr@loc< fun e -> match e with [ $list:([patt; unexpected])$ ] >>

  let handle_missing_field name = function
    | <:ctyp@loc< option $tp$ >> ->
      <:expr@loc< None >>
    | tp -> 
      let loc = Ast.loc_of_ctyp tp in
      <:expr@loc< Bson.bson_error "Missing: %S" $str:name$ >>

  let rec type_of_bson = function
    | <:ctyp@loc< string >> ->
      mk_type_of_bson loc <:match_case@loc< Bson.String s -> s >>
    | <:ctyp@loc< float >> ->
      mk_type_of_bson loc <:match_case@loc< Bson.Double f -> f >>
    | <:ctyp@loc< int32 >> ->
      mk_type_of_bson loc <:match_case@loc< Bson.Int32 v -> v >>
    | <:ctyp@loc< int64 >> ->
      mk_type_of_bson loc <:match_case@loc< Bson.Int64 v -> v >>
    | <:ctyp@loc< bool >> ->
      mk_type_of_bson loc <:match_case@loc< Bson.Boolean b -> b >>
    | <:ctyp@loc< list $tp$ >> ->
      mk_type_of_bson loc <:match_case@loc< 
        Bson.Array l -> List.map $type_of_bson tp$ l 
      >>
    | <:ctyp@loc< array $tp$ >> ->
      mk_type_of_bson loc <:match_case@loc<
        Bson.Array l -> Array.of_list (List.map $type_of_bson tp$ l)
      >>
    | <:ctyp@loc< option $tp$ >> -> 
      <:expr@loc< fun e -> Some ($type_of_bson tp$ e) >>
    | tp -> Gen.unknown_type tp "type_of_bson"

  let record_of_bson tp =
    let loc  = Ast.loc_of_ctyp tp in 
    let bson = Gen.gensym ~prefix:"bson" () in

    let aux (name, field_tp) =
      <:rec_binding@loc< $lid:name$ = 
        try 
          let f = $type_of_bson field_tp$ in 
          f (Bson.Document.find $str:name$ $lid:bson$)
        with 
          [ Not_found -> $handle_missing_field name field_tp$ ]
      >>
    in

    let bindings = List.map ~f:aux (Inspect.fields tp) in
    <:expr@loc< 
      fun [ s -> let $lid:bson$ = Bson.of_string s in { $list:bindings$ } ] 
    >>

  let td_of_bson loc type_name tps rhs =
    let unsupported = (fun _ _ -> raise_unsupported ()) in
    let fun_body = Gen.switch_tp_def
      ~alias:    unsupported
      ~sum:      unsupported
      ~variants: unsupported
      ~mani:     (fun _ _ _ -> raise_unsupported ())
      ~nil:      (fun _ -> raise_unsupported ())
      ~record:   (fun (_ : Loc.t) tp -> record_of_bson tp)
      rhs
    and fun_name = <:patt@loc< $lid:type_name ^ "_of_bson"$ >> in

    <:binding@loc< $fun_name$ = $fun_body$ >>

  let rec tds_of_bson = function
    | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
      td_of_bson loc type_name tps rhs
    | Ast.TyAnd (loc, tp1, tp2) ->
      <:binding@loc< $tds_of_bson tp1$ and $tds_of_bson tp2$ >>
    | _ -> assert false  (* impossible *)

  let of_bson tds =
    let (binding, recursive, loc) =
      match tds with
        | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
            (td_of_bson loc type_name tps rhs,
             Gen.type_is_recursive type_name rhs,
             loc)
        | Ast.TyAnd (loc, _, _) as tds -> (tds_of_bson tds, true, loc)
        | _ -> assert false
    in
      if recursive then
        <:str_item@loc< value rec $binding$ >>
      else
        <:str_item@loc< value $binding$ >>
end


module Gen_bson_of = struct
  let rec bson_of_type = function
    | <:ctyp@loc< string >> ->
      <:expr@loc< Bson.Build.string >>
    | <:ctyp@loc< float >> ->
      <:expr@loc< Bson.Build.double >>
    | <:ctyp@loc< int32 >> ->
      <:expr@loc< Bson.Build.int32 >>
    | <:ctyp@loc< int64 >> ->
      <:expr@loc< Bson.Build.int64 >>
    | <:ctyp@loc< bool >> ->
      <:expr@loc< Bson.Build.boolean >>
    | <:ctyp@loc< list $tp$ >> ->
      <:expr@loc<
        fun l -> Bson.Build.array (List.map $bson_of_type tp$ l)
      >>
    | <:ctyp@loc< array $tp$ >> ->
      <:expr@loc<
        fun a -> Bson.Build.array (Array.(to_list (map $bson_of_type tp$ a)))
      >>
    | <:ctyp@loc< option $tp$ >> ->
      <:expr@loc<
        fun [ Some v -> $bson_of_type tp$ v
            | None   -> Bson.Build.null ]
      >>
    | tp -> Gen.unknown_type tp "bson_of_type"

  let bson_of_record tp =
    let aux loc (name, field_tp) =
      (* TODO: does mutability make any difference? *)
      let bson_tp = bson_of_type field_tp in
      <:expr@loc< ($str:name$, $bson_tp$ $lid:name$) >>
    in

    let loc = Ast.loc_of_ctyp tp in
    let fields = Inspect.fields tp in
    let fun_body = Gen.mk_expr_lst loc (List.map fields ~f:(aux loc))
    and fun_args = List.map fields
      ~f:(fun (n, _) -> <:patt@loc< $lid:n$ = $lid:n$ >>)
    in

    <:expr@loc<
      fun { $list:fun_args$ } ->
        Bson.to_string (Bson.Document.of_list $fun_body$)
    >>

  (* Generate code from type definition. *)
  let bson_of_td loc type_name tps rhs =
    let unsupported = (fun _ _ -> raise_unsupported ()) in
    let fun_body = Gen.switch_tp_def
      ~alias:    unsupported
      ~sum:      unsupported
      ~variants: unsupported
      ~mani:     (fun _ _ _ -> raise_unsupported ())
      ~nil:      (fun _ -> raise_unsupported ())
      ~record:   (fun (_ : Loc.t) tp -> bson_of_record tp)
      rhs
    and fun_name = <:patt@loc< $lid:"bson_of_" ^ type_name$ >> in

    <:binding@loc< $fun_name$ = $fun_body$ >>

  let rec bson_of_tds = function
    | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
        bson_of_td loc type_name tps rhs
    | Ast.TyAnd (loc, tp1, tp2) ->
        <:binding@loc< $bson_of_tds tp1$ and $bson_of_tds tp2$ >>
    | _ -> assert false

  let bson_of tds =
    let (binding, recursive, loc) =
      match tds with
        | Ast.TyDcl (loc, type_name, tps, rhs, _cl) ->
            (bson_of_td loc type_name tps rhs,
             Gen.type_is_recursive type_name rhs,
             loc)
        | Ast.TyAnd (loc, _, _) as tds -> (bson_of_tds tds, true, loc)
        | _ -> assert false
    in
      if recursive then
        <:str_item@loc< value rec $binding$ >>
      else
        <:str_item@loc< value $binding$ >>
end


let () =
  Pa_type_conv.add_generator
    "bson"
    (fun tds ->
      let loc = Ast.loc_of_ctyp tds in
      <:str_item@loc<
        $Gen_of_bson.of_bson tds$;
        $Gen_bson_of.bson_of tds$
      >>
    )
