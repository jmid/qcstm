all:
	ocamlbuild -use-ocamlfind -package qcheck src/qCSTM.cma
	ocamlbuild -use-ocamlfind -package qcheck src/qCSTM.cmxa

install: all
	ocamlfind install qcstm META _build/src/qCSTM.cmi _build/src/qCSTM.cma _build/src/qCSTM.cmxa

uninstall:
	ocamlfind remove qcstm

doc:
	ocamlbuild -use-ocamlfind -package qcheck -docflags -d,doc src/qCSTM.docdir/index.html

examples: queue	counter waterjug stk hashtable buf putget stdio cq

queue:
	ocamlbuild -I src -use-ocamlfind -package qcheck,ppx_deriving.show examples/q.cma examples/q.native

counter:
	ocamlbuild -I src -use-ocamlfind -package qcheck,ppx_deriving.show examples/counter.cma examples/counter.native

waterjug:
	ocamlbuild -I src -use-ocamlfind -package qcheck,ppx_deriving.show examples/waterjug.cma examples/waterjug.native

stk:
	ocamlbuild -I src -use-ocamlfind -package qcheck,ppx_deriving.show examples/stk.cma examples/stk.native

hashtable:
	ocamlbuild -I src -use-ocamlfind -package qcheck,ppx_deriving.show examples/hashtable.cma examples/hashtable.native

buf:
	ocamlbuild -I src -use-ocamlfind -package qcheck,ppx_deriving.show examples/buf.cma examples/buf.native

putget: putgetstub.so
	ocamlbuild -I src -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,ppx_deriving.show examples/putget.native

putgetstub.so: examples/putgetstub.c
	if [[ ! -d _build ]]; then mkdir _build; fi
	if [[ ! -d _build/src ]]; then mkdir _build/examples; fi
	gcc -shared -o _build/examples/putgetstub.so -fPIC examples/putgetstub.c

stdio: stdiostub.o
	ocamlbuild -I src -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,ppx_deriving.show -lflags -custom,examples/stdiostub.o examples/stdio.cma examples/stdio.top
	ocamlbuild -I src -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,ppx_deriving.show -lflags examples/stdiostub.o examples/stdio.native

stdiostub.o: examples/stdiostub.c
	ocamlbuild examples/stdiostub.o

cq: cqstub.o
	ocamlbuild -I src -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,ppx_deriving.show -lflags -custom,examples/cqstub.o examples/cq.cma examples/cq.top
	ocamlbuild -I src -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,ppx_deriving.show -lflags examples/cqstub.o examples/cq.native

cqstub.o: examples/cqstub.c
	ocamlbuild examples/cqstub.o

clean:
	ocamlbuild -clean
	rm -rf doc
	rm -f data.dat
