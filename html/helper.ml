(*
 * This File is part of the FloatView repository.
 * This file was part of the landmarks-viewer project.
 * Copyright (C) 2016 - LexiFi. (under MIT license)
 * Copyright (C) 2025 - Josselin Giet
 *
 * This file is distributed unde the terms of the CecILL v2.1.
 * For more informations, see the LICENSE file.
 *)

open Js_core

module Helper = struct
  let document = Window.document GlobalVariables.window
  let removeAll element =
    while
      match Node.last_child element with
      | Some child -> Node.remove_child element child; true
      | None -> false
    do () done

  let create ?text ?class_name ?style ?id name =
    let element = Document.create_element document name in
    (match text with
     | Some text -> Node.set_text_content element text
     | _ -> ());
    (match style with
     | Some style -> Element.set_attribute element "style" style
     | _ -> ());
    (match id with
     | Some id -> Element.set_attribute element "id" id
     | _ -> ());
    (match class_name with
     | Some class_name -> Element.set_class_name element class_name
     | _ -> ());
    element

  let element_of_id id =
    match Document.get_element_by_id document id with
    | Some element -> element
    | None -> Log.error "Element of id '%s' not found" id

  let hide element =
    Element.set_attribute element "style" "display: none"

  let show element =
    Element.remove_attribute element "style"
end