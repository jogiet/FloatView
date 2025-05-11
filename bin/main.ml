(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

open Floats
module F = Floats
module B = Binary
module D = Decimal
open Convert
open Utils

(** Main module of the progam. *)

let all_precision = [single; double]

let handle input =
  let () = Log.output "%7s: %a" "native" pp_float (Float.of_string input) in
  let d = D.from_string input in
  let b = dec_to_bin d in
  let () = Log.output "%7s: %a" "binary" B.pp b in
  List.iter
    (fun p ->
      let f = bin_to_float p b in
      let d = f |> float_to_bin |> bin_to_dec in
      Log.output "%7s: @[<v 0>%a@ %a@]" p.name
        F.pp_ul f D.pp d)
    all_precision

let _ =
  let _ = Printexc.record_backtrace true in
  let input = Options.init () in
  handle input