(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

(** Minimalistic logging library for interacting with console and alert with
    {!Format} style functions. *)

open Js_core
module Fmt = Format

type out_chan =
  | CONSOLE
  | ALERT

let make_prefixed_multiline_formatter chan =
  let module B = Buffer in
  let buffer = Buffer.create 80 in
  let out s _ _ =
    B.add_string buffer s in
  let flush () =
    let string = Buffer.contents buffer in
    match chan with
    | CONSOLE -> Console.log_string  GlobalVariables.console string
    | ALERT -> GlobalVariables.alert string in
  Fmt.make_formatter out flush

let logk (type a b) chan (k: Fmt.formatter -> a)
    (fmt: (b, Fmt.formatter, unit, a) format4): b =
  (* output_string out prfx; *)
  let formatter = make_prefixed_multiline_formatter chan in
  Fmt.kfprintf k formatter fmt

let flush_fmt formatter =
  Fmt.pp_print_flush formatter ()

let log (fmt: ('a, Fmt.formatter, unit) format): 'a =
  logk CONSOLE flush_fmt fmt

let error (type a b) (fmt: (a, Fmt.formatter, unit,b) format4): a =
  let flush_exit fmt = flush_fmt fmt; failwith "Exit" in
  logk ALERT flush_exit fmt

let log_obj: Ojs.t -> unit =
  Console.log GlobalVariables.console