(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

module F = Floats
module B = Binary
module D = Decimal

open Js_core
open Helper
open Utils

(** {2 Precision} *)

module StringMap = Map.Make(String)

let precisions =
  let add_precision (p: F.float_precision) map = StringMap.add p.name p map in
  StringMap.empty
  |> add_precision F.mini
  |> add_precision F.half
  |> add_precision F.single
  |> add_precision F.double

(** {1 GLobal parameters} *)

(** Precision used currently. *)
let current_precision = ref F.half

(** Decimal value as parsed from the input HTML div. *)
let current_dec = ref (D.from_int 0)

(** Float vlaue currently displayed. *)
let current_float = ref (F.get_zero ~sign:false !current_precision)

(** Current decimal pretty printer. *)
let pp_dec = ref (D.pp_scientific ~significand:10)

(** [update_decimal ()] prints in element [dec_out] and [err_out], the value
    of {!current_float} and the error compared to {!current_dec}. *)
let update_decimal (): unit =
  let n_dec = Helper.element_of_id "dec_out" in
  let n_err = Helper.element_of_id "err_out" in
  try
    let b = !current_float |> Convert.float_to_bin in
    let d = Convert.bin_to_dec b in
    let txt = Format.asprintf "%a" !pp_dec d in
    Node.set_text_content n_dec txt;
    let d_err = D.sub d !current_dec in
    let txt = Format.asprintf "%a" !pp_dec d_err in
    Node.set_text_content n_err txt;
  with
  | Convert.IsInf true ->
      Node.set_text_content n_dec "+Inf";
      Node.set_text_content n_err "+Inf"
  | Convert.IsInf false ->
      Node.set_text_content n_dec "-Inf";
      Node.set_text_content n_err "-Inf"
  | Convert.IsNan ->
      Node.set_text_content n_dec "Nan";
      Node.set_text_content n_err "Nan"

(** [inverse_bit i] inverse the vlaue of the [i]th bit in {!current_float}.
    Later, it calls {!update_decimal}. *)
let inverse_bit i: unit =
  let { s; e; m; p } : F.float_bw = !current_float in
  let len = F.length p in
  begin
  if i < 0 || i >= len then
    Log.error "Cannot update bit %i of float with size %i"
      i len
  else
    if i = len -1 then
      let s = not s in
      current_float := { !current_float with s }
    else if i < p.mantissa then
      let i = p.mantissa - i - 1 in
      Bitv.set m i (not (Bitv.get m i))
    else
      let i = i - p.mantissa in
      Bitv.set e i (not (Bitv.get e i));
  end;
  update_decimal ()

(** [set_all bool] sets all bits of {!current_float} to [bool].
    It calls {!update_decimal} subsequently. *)
let set_all bool =
  let { e; m; p; _ } : F.float_bw = !current_float in
  let s = not bool in
  Bitv.fill e 0 p.exp bool;
  Bitv.fill m 0 p.mantissa bool;
  current_float := { !current_float with s };
  update_decimal ()

(** [add_float table] prints in [table] the value of {!current_float}.
    It sets the [onclick] events. *)
let add_float (table: Ojs.t Node.t) =
  let tr = Helper.create ~class_name:"float" "tr" in
  let dbclick td (): unit =
    let txt = Node.text_content td in
    let txt = if txt = "0" then "1" else "0" in
    let children = Node.children tr in
    Array.iter
      (fun n -> Element.set_attribute n "status" txt;
        Node.set_text_content n txt)
      children;
    set_all (txt = "1") in
  let click td i e: unit =
    if Element.detail e > 1 then dbclick td () else
    let txt = Node.text_content td in
    let txt = if txt = "0" then "1" else "0" in
    Node.set_text_content td txt;
    Element.set_attribute td "status" txt;
    inverse_bit i in
  Helper.removeAll table;
  (* print sign *)
  let f = !current_float in
  let text = if f.s then "0" else "1" in
  let td_sign = Helper.create "td" ~class_name:"sign" ~text in
  Element.set_attribute td_sign "status" text;
  Element.set_attribute td_sign "title"
    (Format.sprintf "%ith bit\nsign bit" (F.length f.p -1));
  Element.set_onclick td_sign (click td_sign (f.p.exp + f.p.mantissa));
  Node.append_child tr td_sign;
  (* print exponent *)
  let last = Bitv.fold_right
    (fun v i ->
      let text = if v then "1" else "0" in
      let td = Helper.create "td" ~class_name:"exp" ~text in
      Element.set_attribute td "title"
        (Format.sprintf "%ith bit\n%ith exponent bit" i (i-f.p.mantissa));
      Element.set_attribute td "status" text;
      Element.set_onclick td (click td i);
      Element.set_ondbclick td (dbclick td);
      Node.append_child tr td;
      i-1)
    f.e (f.p.exp+f.p.mantissa-1) in
  (* print mantissa *)
  let _ = Bitv.fold_left
    (fun i v ->
      let text = if v then "1" else "0" in
      let td = Helper.create "td" ~class_name:"mantissa" ~text in
      Element.set_attribute td "title"
        (Format.sprintf "%ith bit\n%ith mantissa bit" i i);
      Element.set_attribute td "status" text;
      Element.set_onclick td (click td i);
      Element.set_ondbclick td (dbclick td);
      Node.append_child tr td;
      i-1)
    last f.m in
  Node.append_child table tr

(** {2 Decimal value}*)

(** [set_input] inserts in [e] the nodes used to display the decimal value of
    the float as well as the error value. *)
let set_input e =
  let container = Helper.create "div" ~class_name:"container" in
  let usage = Helper.create "div" ~class_name:"usage" ~text:"input" in
  Node.append_child container usage;
  let input = Helper.create "div" ~class_name:"dec_input" ~id:"dec_in" ~text:"0" in
  let click _ =
    Element.set_content_editable input true in
  let blur a =
    let code = Element.key a in
    if code <> "Enter" && code <> "Escape" then () else
    let str = Node.text_content input in
    let str = remove_whitespaces str in
    Node.set_text_content input str;
    try
      let d = D.from_string str in
      current_dec := d;
      let b = Convert.dec_to_bin d in
      let f = Convert.bin_to_float !current_precision b in
      current_float := f;
      let table = Helper.element_of_id "float_repr" in
      add_float table;
      update_decimal ()
    with exn ->
      Node.set_text_content input "0";
      Log.error "Error: %s" (Printexc.to_string exn) in
  Element.set_onclick input click;
  Element.set_onkeyup input blur;
  Node.append_child container input;
  Node.append_child e container

(** [add_decimal e f] inserts a the nodes to display the decimal value and the
    error as children of [e]. *)
let add_decimal e =
  let container = Helper.create "div" ~class_name:"container" in
  let usage = Helper.create "div" ~class_name:"usage" ~text:"Decimal value" in
  Node.append_child container usage;
  let pre_decimal = Helper.create "pre" ~class_name:"dec_output" ~id:"dec_out" in
  let d = !current_float |> Convert.float_to_bin |> Convert.bin_to_dec in
  Node.set_text_content pre_decimal (Format.asprintf "%a" D.pp d);
  Node.append_child container pre_decimal;
  Node.append_child e container;
  let container = Helper.create "div" ~class_name:"container" in
  let usage = Helper.create "div" ~class_name:"usage" ~text:"Error value" in
  Element.set_attribute usage "title" "error = value - input";
  Node.append_child container usage;
  let pre_error = Helper.create "pre" ~class_name:"dec_output" ~id:"err_out" in
  let err = D.sub d !current_dec in
  Node.set_text_content pre_error (Format.asprintf "%a" D.pp err);
  Node.append_child container pre_error;
  Node.append_child e container

let available_precisions = [
  "mini";
  "half";
  "single";
  "double";
]

let set_precision_selection e =
  let container = Helper.create "div" ~class_name:"container" in
  let usage = Helper.create "div" ~class_name:"usage" ~text:"Select precision" in
  let select = Helper.create "select" ~id:"precision" in
  List.iter
    (fun p ->
      let option = Helper.create "option" ~text:p in
      let p = StringMap.find p precisions in
      let title = Format.asprintf "mantissa: %i bits\nexponent: %i bits\nbiais: %i"
        p.mantissa p.exp p.biais in
      Element.set_attribute option "title" title;
      Node.append_child select option)
    available_precisions;
  Node.append_child container usage;
  Node.append_child container select;
  Node.append_child e container;
  let change (): unit =
    let idx = Element.selected_index select in
    let options = Element.options select in
    let option = options.(idx) in
    let text = Node.text_content option in
    let precision = StringMap.find text precisions in
    let decimal = Helper.element_of_id "dec_in" in
    let str = Node.text_content decimal in
    try
      let d = D.from_string str in
      let b = Convert.dec_to_bin d in
      let f = Convert.bin_to_float precision b in
      current_precision := precision;
      current_float := f;
      let table = Helper.element_of_id "float_repr" in
      add_float table;
      update_decimal ()
    with exn ->
      Node.set_text_content decimal "0";
      Log.error "Issue with setting value %s" (Printexc.to_string exn) in
  Element.set_onchange select change



let add_significand e =
  let parent = Helper.create "div" ~class_name:"container" in
  let usage = Helper.create "div" ~class_name:"usage" ~text:"Significand digits" in
  Element.set_attribute usage "title"
    "number of significand digit in the decimal output\n\
     Use 0 for unlimited significand\n\
     Use -1 to not use scientific notation";
  let area = Helper.create "div" ~class_name:"input" ~id:"significand" ~text:"10" in
  Element.set_content_editable area true;
  let update_significand i =
    begin
    if i < 0 then
      pp_dec := D.pp
    else if i = 0 then
      pp_dec := D.pp_scientific
    else
      pp_dec := D.pp_scientific ~significand:i
    end;
    update_decimal () in
  let update_enter e =
    let code = Element.key e in
    if code <> "Enter" then () else
    let str = Node.text_content area in
    let str = remove_whitespaces str in
    Node.set_text_content area str;
    let var = int_of_string str in
    update_significand var in
  Element.set_onkeyup area update_enter;
  let button = Helper.create "button" ~text:"update" in
  let update _ =
    let txt = Node.text_content area in
    let var = int_of_string txt in
    update_significand var in
  Element.set_onclick button update;
  Node.append_child parent usage;
  Node.append_child parent area;
  Node.append_child parent button;
  Node.append_child e parent


(** {2 Onload} *)

let onload _ = begin
  let main = Helper.element_of_id "main" in
  set_precision_selection main;
  set_input main;
  let table = Helper.create "table" ~class_name:"float" ~id:"float_repr" in
  Node.append_child main table;
  add_float table;
  add_decimal main;
  add_significand main
end

let () = Window.set_onload GlobalVariables.window onload