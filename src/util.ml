(* pipelining *)
let ( |> ) x f = f x
let ( <| ) f x = f x
let ( & ) f x = f x

(* composition, don't forget about -no-quot *)
let ( << ) f g x = f (g x)
let ( >> ) f g x = g (f x)
let ( >>>) f g x y = g (f x y)
let ( % ) f g = fun x -> f (g x)
let ( %% ) f g = fun x y -> f (g x y)
let ( %%% ) f g = fun x y z -> f (g x y z)


let flip f x y = f y x
let double x = (x, x)

(* combinators *)
let k_comb x _y = x
let s_comb x y z = x z (y z);;

(* shortcuts for some of the Binary functions. *)
let pack_int32 n =
  let buf = String.create 4 in begin
    Binary.LE.pack_signed_32 ~buf ~pos:0 n;
    buf
  end
and unpack_int32 buf = Binary.LE.unpack_signed_32 ~buf ~pos:0

let pack_int64 v =
  let buf = String.create 8 in begin
    Binary.LE.pack_signed_64 ~buf ~pos:0 v;
    buf
  end
and unpack_int64 buf = Binary.LE.unpack_signed_64 ~buf ~pos:0

let pack_float f =
  let buf = String.create 8 in begin
    Binary.LE.pack_float ~buf ~pos:0 f;
    buf
  end
and unpack_float buf = Binary.LE.unpack_float ~buf ~pos:0


module Stream = struct
  include Stream

  let rec take n s =
    if n > 0
    then icons (next s) & slazy (fun _ -> take (n-1) s)
    else sempty

  let rec take_int32 n s =
    if n > 0l
    then icons (next s) & slazy (fun _ -> take_int32 (Int32.sub n 1l) s)
    else sempty

  (* from BatStream *)
  let rec take_while f s =
    slazy (fun _ -> match peek s with
      | Some h -> junk s;
        if f h
        then icons h (slazy (fun _ -> take_while f s))
        else sempty
      | None -> sempty)

  let to_list s =
    let buf = ref [] in
    (iter (fun x -> buf := x :: !buf) s; List.rev !buf)

  let to_string ?(len=16) s =
    let buf = Buffer.create len in
    iter (fun ch -> Buffer.add_char buf ch) s; Buffer.contents buf

  let to_string_fun fn s =
    let buf = Buffer.create 16 in
    iter (fun it -> Buffer.add_string buf <| fn it ) s; Buffer.contents buf

  let take_string = take >>> to_string
  let take_string_int32 = take_int32 >>> to_string
end


let list_length_int32 l = List.length l |> Int32.of_int

let list_unfold cons pred start =
  let rec loop step acc =
      if pred step then acc
      else let (value, step') = cons step in
           loop step' (value::acc)
  in loop start []

let range ?(start=0) stop = list_unfold (pred >> double) ((==) start) stop

let str_length_int32 s = Int32.of_int & String.length s

(* Ocaml batteries *)
type buffer = { mutable buffer : string;
                mutable position : int;
                mutable length : int;
                initial_buffer : string
              }

external buffer_of_t : Buffer.t -> buffer = "%identity"
external t_of_buffer : buffer -> Buffer.t = "%identity";;

let buffer_change_substring buf pos str =
  let b = buffer_of_t buf in
  String.blit str 0 b.buffer pos (String.length str)
