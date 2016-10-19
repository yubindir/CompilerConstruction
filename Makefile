gram2_test.native: src/gram2_test.ml
	ocamlbuild -use-menhir -use-ocamlfind src/gram2_test.native
