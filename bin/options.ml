(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

(** Module to interact with the command line. *)

module A = Arg
module L = Log
module LL = L.LogLevel

let set_ll lvl () =
  LL.printed_level := lvl

let spec = [
  "-v", (A.Unit (set_ll LL.DEBUG)), " Run the script in verbose mode";
  "-s", (A.Unit (set_ll LL.SUCCESS)), " Run the script in silent mode";
] |> A.align

let usage = "usage: main [options] <float>"

let input = ref ""

let init () =
  A.parse spec (fun s -> input := s) usage;
  if !input = "" then (A.usage spec usage; exit 1) else
  !input