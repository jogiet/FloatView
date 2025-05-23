(*
 * This File is part of the FloatView repository
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed under the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

(** [remove_whitespaces str] returns a copy of str where all whitespaces
    charcters [' ', '\n', '\t'] are removed. *)
let remove_whitespaces str =
  let bytes = Bytes.create (String.length str) in
  let i = String.fold_left
    (fun i c -> if c=' ' || c = '\n' || c = '\t'
      then i
      else (Bytes.set bytes i c; i+1) )
    0 str in
  Bytes.sub_string bytes 0 i
