(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

module LogLevel = struct

  type t =
    | OUTPUT
    | FATAL
    | TODO
    | ERROR
    | SUCCESS
    | WARN
    | INFO
    | PROGRESS
    | DEBUG

  let to_int = function
    | OUTPUT   -> 11
    | FATAL    -> 10
    | TODO     ->  9
    | ERROR    ->  8
    | SUCCESS  ->  7
    | WARN     ->  6
    | INFO     ->  5
    | PROGRESS ->  4
    | DEBUG    ->  0
  let printed_level = ref INFO

  let printable lvl =
    to_int lvl >= to_int !printed_level

  let make_prefix = function
    | OUTPUT   -> ""
    | FATAL    ->
        Format.asprintf "%a%a[FATAL]%a "
          Color.pp_effect Color.Red Color.pp_effect Color.Bold Color.pp_effect Color.Reset
    | TODO     ->
        Format.asprintf "%a%a[TODO ]%a "
          Color.pp_effect Color.Red Color.pp_effect Color.Bold Color.pp_effect Color.Reset
    | ERROR    ->
        Format.asprintf "%a%a[ERROR]%a "
          Color.pp_effect Color.Red Color.pp_effect Color.Bold Color.pp_effect Color.Reset
    | SUCCESS  ->
        Format.asprintf "%a%a[ OK  ]%a "
          Color.pp_effect Color.Green Color.pp_effect Color.Bold Color.pp_effect Color.Reset
    | WARN     ->
        Format.asprintf "%a%a[WARN ]%a "
          Color.pp_effect Color.Yellow Color.pp_effect Color.Bold Color.pp_effect Color.Reset
    | INFO     ->
        Format.asprintf "%a%a[INFO ]%a "
          Color.pp_effect Color.Blue Color.pp_effect Color.Bold Color.pp_effect Color.Reset
    | PROGRESS ->
        Format.asprintf "%a%a[PROGR]%a "
          Color.pp_effect Color.Grey Color.pp_effect Color.Bold Color.pp_effect Color.Reset
    | DEBUG    ->
        Format.asprintf "%a[DEBUG]%a "
          Color.pp_effect Color.Bold Color.pp_effect Color.Reset

  let get_out_channel = function
    | OUTPUT -> stdout
    | _ -> stderr

end

let null_formatter =
  let out _s _index _len = () in
  let flush () = () in
  Format.make_formatter out flush

let make_prefixed_multiline_formatter out_channel prefix =
  let out s index len =
    for i = 0 to len - 1 do
      let c = s.[index + i] in
      if c = '\n' then
        begin
          output_string out_channel "\n";
          output_string out_channel prefix
        end
      else
        output_char out_channel c
    done in
  let flush () =
    flush out_channel in
  Format.make_formatter out flush

let logk (type a b) (k: Format.formatter -> a) (lvl: LogLevel.t)
    (fmt: (b, Format.formatter, unit, a) format4): b =
  if not (LogLevel.printable lvl) then Format.ikfprintf k null_formatter fmt else
  let out = LogLevel.get_out_channel lvl in
  let prfx = LogLevel.make_prefix lvl in
  if prfx <> "" then output_string out prfx;
  (* output_string out prfx; *)
  let formatter = make_prefixed_multiline_formatter out prfx  in
  Format.kfprintf k formatter fmt

let flush_fmt formatter =
  Format.pp_print_flush formatter ();
  output_string stderr "\n";
  flush stderr

let log (lvl: LogLevel.t) (fmt: ('a, Format.formatter, unit) format): 'a =
  logk flush_fmt lvl fmt

let log_exit (type a b) (lvl: LogLevel.t) (fmt: (a, Format.formatter, unit,b) format4): a =
  let flush_exit fmt = flush_fmt fmt; failwith "Exit" in
  logk flush_exit lvl fmt

let output fmt = log OUTPUT fmt
let fail fmt = log_exit FATAL fmt
let todo = fun fmt -> log_exit TODO fmt
let error fmt = log ERROR fmt
let success fmt = log SUCCESS fmt
let warn  fmt = log WARN  fmt
let info fmt = log INFO fmt
let debug fmt = log DEBUG fmt
let log fmt = log PROGRESS fmt
