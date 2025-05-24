(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

module Z = Utils.Z

(** Module used for exact representation and operations of decimal values. *)

(** {1 Arbitrary decimal representation} *)

(** exact representation of decimal numbers *)
type decimal = {
  sign: bool;     (** Sign of tha value. [true] for positive value. *)
  int_part: Z.t;  (** integral part of the decimal number.
                      Always positive.  *)
  frac_part: Z.t; (** fractional part of the decimal number.
                      Always positive. *)
  size_frac: int; (** size of the fractional part {i i.e.} the number of digits
                      after the point.*)
}
(** The value reprsented by a decimal type is
    (-1)·([integer_part] + [frac_part]·10^[-size_frac]).
    *)

(** [extand_frac s size] extend the decimal value [d] so that its fractional
    part has size [size].

    @raise {!Assertion_failure} if the fractional part of [d] is greater than
      [size]. *)
let extend_frac d size =
  assert (size >= d.size_frac);
  if size = d.size_frac then d else
  let padd = Z.pow10 (size-d.size_frac) in
  let frac_part = Z.mul d.frac_part padd in
  { d with size_frac = size; frac_part }

(** [propagate_sign d] set the sign of [d] to its integer and fractional parts.
    This entails that the returned decimal value is not in normal form. *)
let propagate_sign ({sign; int_part; frac_part; _} as d) =
  if sign then d else
  let int_part = Z.neg int_part in
  let frac_part = Z.neg frac_part in
  { d with int_part; frac_part}


(** Addition of two decimal values. *)
let add d1 d2 =
  let size_frac = max d1.size_frac d2.size_frac in
  let d1 = extend_frac d1 size_frac |> propagate_sign in
  let d2 = extend_frac d2 size_frac |> propagate_sign in
  let int_part = Z.add d1.int_part d2.int_part in
  let frac_part = Z.add d1.frac_part d2.frac_part in
  let fact = Z.pow10 size_frac in
  let overflow, frac_part = Z.div_rem frac_part fact in
  let int_part = Z.add int_part overflow in
  let int_part, frac_part, sign = match Z.geq int_part Z.zero, Z.geq frac_part Z.zero with
    | true, true -> int_part, frac_part, true
    | false, false -> Z.neg int_part, Z.neg frac_part, false
    | true, false when Z.equal int_part Z.zero ->
        int_part, Z.neg frac_part, false
    | true, false ->
        let int_part = Z.pred int_part in
        let frac_part = Z.add fact frac_part in
        int_part, frac_part, true
    | false, true ->
        let int_part = Z.pred int_part in
        let frac_part = Z.sub fact frac_part in
        Z.neg int_part, frac_part, false in
  { int_part; frac_part; sign; size_frac}

(** Substraction of two decimal values. *)
let sub d1 d2 =
  let d2 = { d2 with sign = not d2.sign} in
  add d1 d2

type t = decimal

let pp fmt d =
  if d.sign then Format.fprintf fmt "+" else Format.fprintf fmt "-";
  Z.pp_print fmt d.int_part;
  if Z.equal Z.zero d.frac_part then () else
  Format.fprintf fmt ".%s"
    (Z.format (Format.sprintf "%%0%ii" d.size_frac) d.frac_part)

(** [pp_scientific ?significand] is a pretty printer of decimal values in
    scientific notation.

    The optional argument [significand] set the number of significand number to
    print. If this argument is not set; or set to [0], then all digits in the
    decimal values are printed. *)
let pp_scientific ?significand fmt d =
  if d.int_part = Z.zero && d.frac_part = Z.zero then
    Format.fprintf fmt "%c0.0"
    (if d.sign then '+' else '-') else
  let pow, str =
    if d.int_part = Z.zero then
      let str = Z.to_string d.frac_part in
      - d.size_frac -1 + String.length str, str
    else
      let int_str = Z.to_string d.int_part in
      let pow = String.length int_str -1 in
      let str = Format.asprintf "%s%s"
          int_str
          (Z.format (Format.sprintf "%%0%ii" d.size_frac) d.frac_part) in
      pow, str in
  let len = match significand with
    | None -> String.length str -1
    | Some i when i <= 0 -> String.length str -1
    | Some i -> min i (String.length str -1 ) in
  Format.fprintf fmt "%c%c.%se%i"
    (if d.sign then '+' else '-')
    (String.get str 0)
    (String.sub str 1 len)
    pow

let from_int i =
  let sign, i = if i >= 0 then true, i else false, -i in
  let int_part = Z.of_int i in
  { sign; int_part; frac_part = Z.zero; size_frac = 0 }

let from_string str =
  let l = String.length str in
  let sign, pos =
    if String.starts_with ~prefix:"+" str then
      true, 1
    else if String.starts_with ~prefix:"-" str then
      false, 1
    else true, 0 in
  match String.index_opt str '.' with
  | None ->
      let int_part = Z.of_substring str ~pos ~len:(l - pos) in
      { sign; int_part; frac_part = Z.zero; size_frac = 0 }
  | Some i ->
      let int_part = Z.of_substring str ~pos ~len:(i-pos) in
      let size_frac = l -i-1 in
      let frac_part = Z.of_substring str ~pos:(i+1) ~len:(l-i-1) in
      { sign; int_part; size_frac; frac_part }

(** [pow10 d p] returns the representation of [d*10^p]. *)
let pow10 d p =
  if p = 0 then d else
  if p > 0 then
    let size_frac = max 0 (d.size_frac - p) in
    let fact = Z.pow10 size_frac in
    let div, frac_part = Z.div_rem d.frac_part fact in
    let fact_int = Z.pow10 p in
    let int_part = Z.add div (Z.mul d.int_part fact_int) in
    { size_frac; int_part; frac_part; sign = d.sign }
  else
    let p = -p in
    let size_frac = p + d.size_frac in
    let fact =  Z.pow10 p in
    let int_part, rem = Z.div_rem d.int_part fact in
    let fact_frac = Z.pow10 d.size_frac in
    let frac_part = Z.add d.frac_part (Z.mul fact_frac rem) in
    { size_frac; int_part; frac_part; sign = d.sign }

(** [from_string s] parses the value in [s].
    It support scientific notation such as [1.1754943508e−38]. *)
let from_string s =
  match String.index_opt s 'e' with
  | None -> from_string s
  | Some idx ->
    let significand = String.sub s 0 idx in
    let exponent = String.sub s (idx+1) (String.length s - idx -1 ) in
    let d = from_string significand in
    pow10 d (int_of_string exponent)