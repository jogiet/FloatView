(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

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


type t = decimal

let pp fmt d =
  if d.sign then () else Format.fprintf fmt "-";
  Z.pp_print fmt d.int_part;
  if Z.equal Z.zero d.frac_part then () else
  Format.fprintf fmt ".%s"
    (Z.format (Format.sprintf "%%0%ii" d.size_frac) d.frac_part)

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
    let fact = Z.of_string ("1"^(String.make size_frac '0')) in
    let div, frac_part = Z.div_rem d.frac_part fact in
    let fact_int = Z.of_string ("1"^(String.make p '0')) in
    let int_part = Z.add div (Z.mul d.int_part fact_int) in
    { size_frac; int_part; frac_part; sign = d.sign }
  else
    let p = -p in
    let size_frac = p + d.size_frac in
    let fact = Z.of_string ("1"^(String.make p '0')) in
    let int_part, rem = Z.div_rem d.int_part fact in
    let fact_frac = Z.of_string ("1"^(String.make d.size_frac '0')) in
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