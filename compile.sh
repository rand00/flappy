
#! /bin/bash

set -e

## JS

jsbyte="flap.byte"
jsout="webout/flap.js"

ocamlbuild \
    -use-ocamlfind \
    -cflags -linkpkg \
    "$jsbyte"

js_of_ocaml "$jsbyte" -o "$jsout"
    # +weak.js +cstruct/cstruct.js \

## HTML

ocamlbuild -use-ocamlfind index_ht.byte
./index_ht.byte > webout/index.html


