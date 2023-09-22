all:
	cat hand.ml search.ml main.ml > c_all.ml
	ocamlc unix.cma c_all.ml -o exe.byte
	ocamlopt.opt -S unix.cmxa c_all.ml -o exe.opt
