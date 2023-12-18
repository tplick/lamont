_default:
	cat bloom.ml fastbits.ml hand.ml search.ml main.ml > c_all.ml
	ocamlc unix.cma c_all.ml -o exe.byte
	ocamlopt.opt -S unix.cmxa c_all.ml -o exe.opt

speedtest:
	zsh -c 'for ((i=0;i<20;i++)); do time ./exe.opt -bench 100 >/dev/null; done'
