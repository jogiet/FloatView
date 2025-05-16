(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

(** Module used for exact representation and operations of binary values. *)

(** {1 Binary} *)

(** exact representation of binay values. *)
type binary = {
  sign: bool;         (** [true] for positive value *)
  integer: Bitv.t;    (** integral part of the value. *)
  fractional: Bitv.t; (** non-repeating part of the fractional part of the value.*)
  repeating: Bitv.t;  (** repeating part of the frctional part of the value. *)
}

let pp fmt {sign; integer; fractional; repeating } =
  if not sign then Format.pp_print_char fmt '-';
  if Bitv.length integer > 0
    then Bitv.M.print fmt integer
    else Format.pp_print_char fmt '0';
  let printed = if true || Bitv.length fractional > 0
    then (Format.fprintf fmt ".%a" Bitv.L.print fractional; true)
    else false in
  if Bitv.length repeating > 0 then
    Format.fprintf fmt "%s(%a)*"
      (if printed then "" else ".")
      Bitv.L.print repeating

let from_int x =
  let sign, x = if x >= 0 then true, x else false, -x in
  let integer = Bitv.of_int_us x in
  let fractional = Bitv.create 0 false in
  let repeating = fractional in
  { sign; integer; fractional; repeating }

let zero = from_int 0

(** [get_bit bin i] returns the i^th bit of binary value [bin]. *)
let get_bit bin i =
  if i >= 0 then
    if Bitv.length bin.integer > i
      then Bitv.get bin.integer i
      else false
  else
    let i = -i -1 in
    let len_under = Bitv.length bin.fractional in
    if i < len_under then
      Bitv.get bin.fractional i
    else
      let i = i - len_under  in
      let len_repeat = Bitv.length bin.repeating in
      if len_repeat = 0 then
        false
      else
        Bitv.get bin.repeating (i mod len_repeat)

exception Found of int

(** [get_first_non_null bin] returns the index of the first non-null bit
    in [bin]. Returns [None] if [bin] is equal to zero. *)
let get_first_non_null bin =
  try
    for i = (Bitv.length bin.integer) - 1 downto 0 do
      if Bitv.get bin.integer i then raise (Found i)
    done;
    for i = 0 to (Bitv.length bin.fractional) - 1 do
      if Bitv.get bin.fractional i then raise (Found (-(i+1)))
    done;
    for i = 0 to (Bitv.length bin.repeating) - 1 do
      if Bitv.get bin.repeating i then raise (Found (-(i+(Bitv.length bin.fractional)+1)))
    done;
    None
  with Found i -> Some i

(** [is_null_after b bit] returns true [true] iff. all bits in [b] after the
    [bit]^th (included) are null.*)
let is_null_after b bit =
  let thresh = -(Bitv.length b.repeating + Bitv.length b.fractional + 1) in
  let rec aux i =
    i < thresh || not (get_bit b i) && aux (i-1) in
  aux bit

(** Alias for the {!binary} type. used for {!Map.Make} functoe. *)
type t = binary

(** Comparison function.
    Follows the order of real values {i i.e.}  [compare b1 b2] is
    + [0] when [b1] is equal to [b2];
    + smaller than [0] if [b1] is lower than [b2];
    + greater than [0] otherwise. *)
let compare b1 b2 =
  match b1.sign, b2.sign with
  | false, true -> 1
  | true, false -> -1
  | _, _ ->
    match get_first_non_null b1, get_first_non_null b2 with
    | None, None -> 0
    | Some _, None -> if b1.sign then 1 else -1
    | None, Some _ -> if b1.sign then -1 else 1
    | Some i1, Some i2 when i1 <> i2 -> if b1.sign then i1 - i2 else i2 - i1
    | Some i, Some _ ->
      let lim = max (Bitv.length b1.fractional) (Bitv.length b1.fractional) in
      let lim = -lim in
      let rec compare i =
        if i < lim then 0 else
        match get_bit b1 i, get_bit b2 i with
        | true, false -> if b1.sign then 1 else -1
        | false, true -> if b1.sign then -1 else 1
        | _, _  -> compare (i-1) in
      compare i
