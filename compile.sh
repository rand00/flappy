
#! /bin/bash

set -e

## JS

jsbyte="gaxel.byte"
jsout="webout/gaxel.js"

ocamlbuild \
    -use-ocamlfind \
    -cflags -linkpkg \
    "$jsbyte"

js_of_ocaml "$jsbyte" -o "$jsout"
    # +weak.js +cstruct/cstruct.js \

## HTML

ocamlbuild -use-ocamlfind index_html.byte

./index_html.byte > webout/index.html


    
