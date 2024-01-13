_default:
	gcc -S -I `ocamlc -where` -O3 pext_neon.c
	gcc -fPIC -I `ocamlc -where` -O3 -shared pext_neon.c -o pext_neon.o
	cat bloom.ml fastbits.ml hand.ml libdeals.ml search.ml main.ml > c_all.ml
	ocamlopt.opt -S -I +unix unix.cmxa pext_neon.o c_all.ml -o exe.opt

speedtest:
	zsh -c 'for ((i=0;i<20;i++)); do time ./exe.opt -bench 100 >/dev/null; done'

benchlite:
	zsh -c 'for ((i=0;i<20;i++)); do time ./exe.opt -bench-lite 100000 >/dev/null; done'
