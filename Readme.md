FloatView: Spending some time experimenting with float representation
====================================================================

This project is heavily inspired by the
[IEEE-754 Visualization](https://bartaz.github.io/ieee754-visualization/)
project by [bartaz](https://github.com/bartaz)

TODO:
[ ] Some optimization for large representation (*i.e.* quadruple and octuple)

Installation
------------

### Install dependancies

```
opam install dune zarith js_of_ocaml bitv zarith_stub_js gen_js_api
```

To use the webpage, you need the version of `Bitv` with support for `js_of_ocaml`:

```
opam pin add bitv https://github.com/nicolasdespres/bitv.git#fix-max-length-overflow
```

### Compile

Simply run `dune build`

License
-------

This project is distributed under CeCILLv2.
See [LICENSE file](LICENSE) for more information.
