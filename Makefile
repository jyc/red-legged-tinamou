.PHONY: all clean test

all: main.byte 

clean:
	ocamlbuild -use-ocamlfind -clean

main.byte: main.ml
	ocamlbuild -use-ocamlfind main.byte

main.native: main.ml
	ocamlbuild -use-ocamlfind main.native

test:
	./run-tests.bash
