(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

(** {1 Float precision} *)

(** Type of float representation*)
type float_precision = {
  mantissa: int; (** physical size of the mantissa {i i.e.} does not include the leading [1] *)
  exp: int;      (** physical size of the exponent *)
  biais: int;    (** biais *)
  name: string;  (** name of the precision ({i e.g.} [single], [double])*)
}


let pp_float_precision fmt {mantissa; exp; biais; _} =
  Format.fprintf fmt "exp: %3i (biais: %10i) mantissa: %i"
    exp biais mantissa

let length {mantissa; exp; _} = 1 + mantissa + exp

let build_precision ?(name = "") mantissa ?(biais=None) exp =
  let biais = match biais with
    | None -> (Int.shift_left 1 (exp-1)) - 1
    | Some biais -> biais in
  { mantissa; exp; biais; name }

(** {2 IEEE-754 representation}*)

let mini = build_precision ~name:"mini" 3 4
let half   = build_precision ~name:"half" 10 5
let single = build_precision ~name:"single" 23 8
let double = build_precision ~name:"double" 52 11
let quadruple = build_precision ~name:"quadruple" 112 15
let octuple = build_precision ~name:"octuple" 236 19

(** precision of the currently sued system.
    May be {!single} (32bits) or {!double} (64 bits) *)
let sys_precision = match Sys.word_size with
  | 32 -> single
  | 64 -> double
  | sz -> Log.fail "Unknown word size: %i" sz

(** {1 Float bitwise representation} *)

(** Explicit bitwise manipulation of a IEEE floating point number. *)
type float_bw = {
  p: float_precision; (** underlying precision used *)
  s: bool;            (** sign bit. [true] for positive value. *)
  e: Bitv.t;          (** exponent bitfield *)
  m: Bitv.t;          (** mantissa bitfield *)
}

type t = float_bw

(** [copy f] returns a deep copy of [f]. *)
let copy {p; s; e; m} =
  let e' = Bitv.copy e in
  let m' = Bitv.copy m in
  {p; s; e = e'; m = m'}

(** {2 Pretty printers } *)

(** Print the float as bitfield. *)
let pp fmt  {s; e; m; _} =
  Format.fprintf fmt "%i%a%a"
    (if s then 0 else 1)
    Bitv.M.print e
    Bitv.L.print m

(** Print the float and underline the exponent. *)
let pp_ul fmt  {s; e; m; _} =
  Format.fprintf fmt "%i%a%a%a%a"
    (if s then 0 else 1)
    Color.pp_effect Color.Uline
    Bitv.M.print e
    Color.pp_effect Color.Reset
    Bitv.L.print m

(** Print the float in a box. *)
let pp_boxed fmt {s; e; m; p} =
  let line = Format.asprintf "+-+%s+%s+"
    (String.make p.exp '-')
    (String.make p.mantissa '-') in
  Format.fprintf fmt "@[<v 0>%s@ |%i|%a|%a|@ %s@]"
    line
    (if s then 0 else 1)
    Bitv.M.print e
    Bitv.L.print m
    line

(** {2 Simple construction function}*)

let of_float f =
  let t = Obj.repr f in
  let () = assert (Obj.tag t = Obj.double_tag) in
  let i: nativeint = Obj.raw_field t 0 in
  let v = Bitv.of_nativeint_s i in
  let p = sys_precision in
  let l = length p in
  let () = assert (Bitv.length v = l) in
  let s = not (Bitv.get v (l-1)) in
  let e = Bitv.init p.exp
    (fun i -> Bitv.get v (p.mantissa + i)) in
  let m = Bitv.init p.mantissa
    (fun i -> Bitv.get v (p.mantissa -1 -i)) in
  { s; e; m; p }

let get_zero ?(sign=true) p =
  let e = Bitv.create p.exp false in
  let m = Bitv.create p.mantissa false in
  { p; s = sign; e; m }

let get_inf sign p =
  let e = Bitv.create p.exp true  in
  let m = Bitv.create p.mantissa false in
  { p; s = sign; e; m }

(** {2 Checking function}*)

(** [is_zero f] returns [true] iff. [f] is [+/-0.0]. *)
let is_zero {e; m; _} =
  (Bitv.all_zeros e) && (Bitv.all_zeros m)

(** [is_nan f] returns [true] iff. [f] is a Nan value. *)
let is_nan {e;m;_} =
  Bitv.all_ones e && not (Bitv.all_zeros m)

(** [is_inf f] returns [true] iff. [f] is +/-Inf. *)
let is_inf {e; m; _} =
  Bitv.all_ones e && Bitv.all_zeros m

(** [is_zero f] returns [true] iff. [f] is a denormalized (or a subnormal)
    floating point value. *)
let is_denormalized {e; _} =
  Bitv.all_zeros e

(** [classify f] returns the floating class of [f].
    See also {!Float.classify_float}*)
let classify {e; m; _} : Float.fpclass =
  if Bitv.all_zeros e then
    if Bitv.all_zeros m
      then FP_zero
      else FP_subnormal
  else if Bitv.all_ones e then
    if Bitv.all_zeros m
      then FP_infinite
      else FP_nan
  else FP_normal

(** {2 Utilitaries functions}*)

exception Overflow

(** [inc_mantissa b] Incrment the mantissa. by starting by the {b most significant bits}.
    @raise {!Overflow} if the mantissa is [1..111] *)
let inc_mantissa m =
  let rec aux i =
    if i < 0 then raise Overflow else
    if Bitv.get m i then begin
      Bitv.set m i false;
      aux (i-1) end
    else
      Bitv.set m i true in
    aux (Bitv.length m - 1)

let dec_mantissa m =
  let rec aux i =
    if i < 0 then raise Overflow else
    if Bitv.get m i then
      Bitv.set m i false
    else begin
      Bitv.set m i true;
      aux (i-1)
    end in
  aux (Bitv.length m - 1)


let inc_exp e =
  let rec aux i =
    if i >= Bitv.length e then raise Overflow else
    if Bitv.get e i then begin
      Bitv.set e i false;
      aux (i+1) end
    else
      Bitv.set e i true in
    aux 0

let dec_exp e =
  let rec aux i =
    if i >= Bitv.length e then raise Overflow else
    if Bitv.get e i then
      Bitv.set e i false
    else begin
      Bitv.set e i true;
      aux (i+1)
    end in
  aux 0

(** [next_inf f] returns the next float in the direction of ±∞ according to the
    sign of [f].
    If [f] is [+Inf] (or [-Inf]) then a {!copy} of [f] is returned. *)
let next_inf f =
  let {e; m; _} as res = copy f in
  if is_inf f then res else
  try inc_mantissa m; res
  with Overflow ->
    let () = assert( Bitv.all_zeros m) in
    try inc_exp e; res
    with Overflow -> Log.fail "Unexpected Overflowx in increment exp"

(** [next_inf f] returns the next float towards [0].
    If [f] is [+0.0] then [next_zeo] returns [-0.0],
    If [f] is [-0.0] then [next_zeo] returns [+0.0], *)
let next_zero f =
  let {e; m; s; _} as res = copy f in
  if is_zero f then { res with s = not s} else
  try dec_mantissa m; res
  with Overflow ->
    let () = assert (Bitv.all_ones m) in
    try dec_exp e; res
    with Overflow -> Log.fail "Unexpected Overflow in decrement exp "

(** [succ f] returns the floating point number right after [f].
    See also {!Float.succ}. *)
let succ f =
  if is_zero f then next_inf {f with s = true} else
  if f.s then next_inf f else next_zero f

(** [pred f] returns the floating point number right before [f].
    See also {!Float.pred}. *)
let pred f =
  if is_zero f then next_inf {f with s = false} else
  if f.s then next_zero f else next_inf f

(** {1 Rounfing mode} *)

type rounding =
  | TO_INF | TO_MINF (** round towards \pm inf *)
  | TO_ZERO    (** rounds towards zero (ignore trailing bits) *)
  | TO_NEAREST (** rounds towards nearest floating point. *)