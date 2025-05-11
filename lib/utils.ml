(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

(** [pp_float fmt f] prints the bitlayout of the float [f] *)
let pp_float fmt (f: float) =
  let t = Obj.repr f in
  let _ = assert (Obj.tag t = Obj.double_tag) in
  let i: nativeint = Obj.raw_field t 0 in
  let v = Bitv.of_nativeint_s i in
  Format.fprintf fmt "%a" Bitv.M.print v
