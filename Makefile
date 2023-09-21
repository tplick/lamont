all:
	cat hand.ml search.ml main.ml > _all.ml
	ocamlc unix.cma _all.ml -o exe.byte
	ocamlopt.opt unix.cmxa _all.ml -o exe.opt
