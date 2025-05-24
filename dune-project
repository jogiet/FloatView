(lang dune 3.17)

(name FloatView)

(generate_opam_files true)

(source
 (github jogiet/FloatView))

(authors "Josselin Giet <josselin.giet@ens.fr>")

(maintainers "Josselin Giet <josselin.giet@ens.fr>")

(license CECILL-2.1)

(documentation https://url/to/documentation) ; TODO

(package
 (name FloatView)
 (synopsis "Float representation visualizer")
 (description "A module to experiment with binary representation of floating point value")
 (depends ocaml bitv zarith zarith_stubs_js gen_js_api)
 (tags
  ("float" "IEEE-754")))
