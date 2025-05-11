(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

(** This module is used to test various functions of modules *)

module F = Floats
module U = Utils
open Convert

(** {1 Main test function} *)

(** [test f bench] applies [f] to all elements of [bench] and summariez the
    tests*)
let test (type a) (f: a -> bool) (bench: a list) =
  let t_beg = Unix.gettimeofday () in
  let failures, size = List.fold_left
    (fun (failures, size) v ->
      try if f v
        then failures, size+1
        else failures + 1, size+1
      with _ -> failures + 1, size+1)
    (0, 0) bench in
  let t_end = Unix.gettimeofday () in
  let dt = t_end -. t_beg in
  if failures = 0 then
    Log.success "All %i tests pass (time: %fs)" size dt
  else
    let rate = (100.0 *. (float_of_int failures) /. (float_of_int size)) in
    Log.error "%i/%i tests fail (rate: %5.2f %%) (time: %fs)" failures size rate
      dt

(** [test_from_float] test the {!F.from_float} function. *)
let test_from_float f =
  let s_caml = Format.asprintf "%a" U.pp_float f in
  let f_custom = F.of_float f in
  let s_custom = Format.asprintf "%a" F.pp f_custom in
  if s_custom = s_caml then
    true
  else
    let () = Log.error
      "@[<v 2>Results do not match:@ s_caml  : %s@ s_custom: %a@]"
      s_caml F.pp_ul f_custom in
    false

(** [test_succ] checks that {!F.succ} behaves as {!Float.succ}. *)
let test_succ f =
  let f_custom = F.of_float f in
  let next_custom = F.succ f_custom in
  let next_ocaml = Float.succ f in
  let s_caml = Format.asprintf "%a" U.pp_float next_ocaml in
  let s_custom = Format.asprintf "%a" F.pp next_custom in
  if s_caml = s_custom then
    true
  else
    let () = Log.error
      "@[<v 2>Results do not match:@ input   : %a@ s_caml  : %s@ s_custom: %a@]"
      F.pp_ul f_custom s_caml F.pp_ul f_custom in
    false

(** [test_pred] checks that {!F.pred} behaves as {!Float.pred}. *)
let test_pred f =
  let f_custom = F.of_float f in
  let next_custom = F.pred f_custom in
  let next_ocaml = Float.pred f in
  let s_caml = Format.asprintf "%a" U.pp_float next_ocaml in
  let s_custom = Format.asprintf "%a" F.pp next_custom in
  if s_caml = s_custom then
    true
  else
    let () = Log.error
      "@[<v 2>Results do not match:@ input   : %a@ s_caml  : %s@ s_custom: %a@]"
      F.pp_ul f_custom s_caml F.pp_ul f_custom in
    false


(** [test_classify] checks that {!F.classify} behaves as {!Float.classify_float}. *)
let test_classify f =
  let pp_class fmt : Float.fpclass -> unit = function
    | FP_normal -> Format.pp_print_string fmt "normal"
    | FP_nan -> Format.pp_print_string fmt "nan"
    | FP_infinite -> Format.pp_print_string fmt "inf"
    | FP_zero -> Format.pp_print_string fmt "zero"
    | FP_subnormal -> Format.pp_print_string fmt "subnormal" in
  let f_custom = F.of_float f in
  let fp_f = Float.classify_float f in
  let fp_custom = F.classify f_custom in
  if fp_f = fp_custom then
    true
  else
    let () = Log.error
      "@[<v 2>Results do not match %a:@ fp_caml  : %a@ fp_custom: %a@]"
      F.pp_ul f_custom
      pp_class fp_f pp_class fp_custom in
    false

let test_conversion f =
  let clas = Float.classify_float f in
  if clas = FP_infinite || clas = FP_nan then true else
  let s_caml = Format.asprintf "%a" U.pp_float f in
  (* TODO: find a way to reduce the cost of the float printing.
   * Is there a way to print it in scientific notation ? *)
  let d = D.from_string (Printf.sprintf "%02200.1100f" f) in
  let b = dec_to_bin d in
  let f = bin_to_float F.sys_precision b in
  let s_custom = Format.asprintf "%a" Floats.pp f in
  if s_caml = s_custom then
    true
  else
    let () = Log.error
      "@[<v 2>Results do not match:@ s_caml  : %s@ s_custom: %a@]"
      s_caml F.pp_ul f in
    let () = Log.info "dec: %a\nbin: %a" D.pp d B.pp b in
    false

let floats_list  =
[
  0.0; Float.max_float; Float.min_float;
  (* 16_777_216 is the last integer that can be precisely converted to single. *)
  16_777_216.0; 16_777_217.0; 16_777_218.0; 16_777_219.0; 16_777_220.0;
  (* 9_007_199_254_740_992 is the last integer that can be precisely converted to double *)
  9_007_199_254_740_992.0; 9_007_199_254_740_993.0; 9_007_199_254_740_994.0;
  0.1; 0.01; 0.001; 0.000_1; 0.000_01; 0.000_001; 0.000_000_1; 0.000_000_01;
  0.000_000_001; 0.000_000_000_1; 0.000_000_000_01; 0.000_000_000_001;
  0.000_000_000_000_1; 0.000_000_000_000_01;
  1.0; 10.0; 100.0; 1_000.0; 10_000.0; 100_000.0; 1_000_000.0; 10_000_000.0;
  100_000_000.0; 1_000_000_000.0
]

(** {!floats_list} contains several float values such as {!Float.max_float},
    {!Float.min_float}, and the smallest integer that cannot be represented
    exactly by single precision floats. *)
let floats_list =
  List.fold_left (fun acc f -> f::(-.f)::acc) [] floats_list

(** {!floats_list_rnd} add randomly chosen elements from threshold defined in
    {!floats_list}. *)
let floats_list_rnd =
  let lim = 1000 in
  let rec aux thresh acc i =
    if thresh = 0.0 || thresh = -0.0 then acc else
    if i >= lim then acc else
    let f = Random.float thresh in
    aux thresh (f::acc) (i+1) in
  List.fold_left
    (fun acc f -> aux f (f::acc) 0 )
    [] floats_list

let _ =
  let () = Log.info "Testing from_float" in
  let () = test test_from_float floats_list_rnd in
  let () = Log.info "Testing succ" in
  let () = test test_succ floats_list_rnd in
  let () = Log.info "Testing pred" in
  let () = test test_pred floats_list_rnd in
  let () = Log.info "Testing classify" in
  let () = test test_classify floats_list_rnd in
  let () = Log.info "Testing conversion" in
  let () = test test_conversion  floats_list_rnd in
  ()