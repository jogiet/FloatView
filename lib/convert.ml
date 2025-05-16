(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

module D = Decimal
module B = Binary
module F = Floats

(** {1 Decimal <-> Binary} *)

let frac_lim = 5000

let dec_to_bin (d: D.t) : B.t =
  let i = ref d.int_part in
  let integer = Bitv.init (Z.numbits d.int_part)
    (fun _ ->
      let res = Z.is_odd !i in
      let () = i := Z.shift_right_trunc !i 1 in
      res) in
  if d.size_frac = 0 then
    let fractional = Bitv.create 0 false in
    let repeating = fractional in
    { sign = d.sign; integer; repeating; fractional }
  else
    let tbl = Hashtbl.create 50 in
    let fractional = Bitv.create frac_lim false in
    let divisor = Z.mul (Z.of_int 5) (Z.pow (Z.of_int 10) (d.size_frac-1)) in
    let rec aux i rem =
      if i >= frac_lim then begin
        Log.warn "Exceeding fractional limite search";
        (i, i) end
      else if Z.equal Z.zero rem then (i, i) else
        let () = Hashtbl.add tbl rem i in
        let b = Z.geq rem divisor in
        let rem = if b then Z.shift_left (Z.sub rem divisor) 1 else Z.shift_left rem 1 in
        let () = Bitv.set fractional i b in
        match Hashtbl.find_opt tbl rem with
        | Some j -> (j, i+1)
        | None -> aux (i+1) rem in
    let lim_frac, lim_rep = aux 0 d.frac_part in
    let repeating = Bitv.sub fractional lim_frac (lim_rep-lim_frac) in
    let fractional = Bitv.sub fractional 0 lim_frac in
    { sign = d.sign; integer; repeating; fractional }

let bin_to_dec (b: B.t): D.t =
  if Bitv.length b.repeating <> 0 then
    (* Maybe do an iterative approximation *)
    Log.todo "Implement conversion for repeating part";
  let int_part = Bitv.fold_right
    (fun bi acc ->
      let acc = Z.shift_left acc 1 in
      if bi then Z.succ acc else acc)
    b.integer Z.zero in
  let ten = Z.of_int 10 in
  let five = Z.of_int 5 in
  let frac_part, _, size_frac = Bitv.fold_left
    (fun (acc, pow, i) b ->
      let acc = Z.mul acc ten in
      let pow = Z.mul pow five in
      let acc = if b then Z.add acc pow else acc in
      (acc, pow, i+1))
    (Z.zero, Z.one, 0) b.fractional in
  { sign = b.sign; int_part; frac_part; size_frac }

(** {1 Binary <-> Floats} *)


let bin_to_float p (bin: B.t) : F.float_bw =
  let sign = bin.sign in
  match B.get_first_non_null bin with None -> F.get_zero ~sign p | Some is ->
  let exp = ref (is + p.biais) in
  if !exp <= 0 then
    if !exp + p.mantissa <= 0 then
      Floats.get_zero ~sign:bin.sign p
    else
      let e = Bitv.create p.exp false in
      let m = Bitv.init p.mantissa (fun i -> B.get_bit bin (-p.biais-i)) in
      let s = bin.sign in
      {p; s; e; m} else
  let e = Bitv.create p.exp false in
  let () =
    for i = 0 to p.exp - 1 do
      if !exp mod 2 <> 0 then Bitv.set e i true;
      exp := !exp / 2;
    done in
  if !exp > 0 || Bitv.all_ones e then F.get_inf bin.sign p else
  let m = Bitv.init p.mantissa
    (fun i -> B.get_bit bin (is - i - 1)) in
  let round = B.get_bit bin (is - p.mantissa -1 ) in
  let _ = if round then try
      Floats.inc_mantissa m
    with F.Overflow ->
      try Floats.inc_exp e
      with F.Overflow -> Log.debug "Impossible Overflow: %a"
        F.pp_ul { p; e; m; s = sign }
  in
  { p; e; m; s = sign }

exception IsInf of bool
exception IsNan

let float_to_bin (f: F.t) =
  let sign = f.s in
  if F.is_zero f then { B.zero with sign } else
  if Bitv.all_ones f.e then
    if Bitv.all_zeros f.m then raise (IsInf f.s) else raise IsNan else
  let exp = Bitv.to_int_us f.e in
  let exp = exp - f.p.biais in
  let len_int, len_frac = if exp >= 0
    then exp + 1, max 0 (f.p.mantissa - exp)
    else 0, -exp + f.p.mantissa in
  let integer = Bitv.create len_int false in
  let fractional = Bitv.create len_frac false in
  let _ =
    for i = 0 to f.p.mantissa do
      let b = if i = 0
        then not (F.is_denormalized f)
        else Bitv.get f.m (i-1) in
      if exp >= 0 then
        if i < len_int then
          Bitv.set integer (len_int-i-1) b
        else
          Bitv.set fractional (i-len_int) b
      else
        Bitv.set fractional (-exp+i-1) b
    done in
  let repeating = Bitv.create 0 false in
  { sign; integer; fractional; repeating }