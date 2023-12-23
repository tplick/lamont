_default:
	cat bloom.ml fastbits.ml hand.ml libdeals.ml search.ml main.ml > c_all.ml
	ocamlc -I +unix unix.cma c_all.ml -o exe.byte
	ocamlopt.opt -S -I +unix unix.cmxa c_all.ml -o exe.opt

speedtest:
	zsh -c 'for ((i=0;i<20;i++)); do time ./exe.opt -bench 100 >/dev/null; done'

benchlite:
	zsh -c 'for ((i=0;i<20;i++)); do time ./exe.opt -bench-lite 100000 >/dev/null; done'
