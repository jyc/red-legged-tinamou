#!/bin/bash

run_test() {
    ocamlbuild -use-ocamlfind $1.d.byte && ./$1.d.byte -runner sequential
}

run_test route_test
