all:
	ocamlbuild -use-ocamlfind -package qcheck src/qCSTM.cma
	ocamlbuild -use-ocamlfind -package qcheck src/qCSTM.cmxa

install: all
	ocamlfind install qcstm META _build/src/qCSTM.cmi _build/src/qCSTM.cma _build/src/qCSTM.cmxa _build/src/qCSTM.a _build/src/qCSTM.cmo _build/src/qCSTM.cmx _build/src/qCSTM.o

uninstall:
	ocamlfind remove qcstm

doc:
	ocamlbuild -use-ocamlfind -package qcheck -docflags -d,doc src/qCSTM.docdir/index.html

examples: queue	counter waterjug stk hashtable buf putget stdio cq

queue:
	ocamlbuild -use-ocamlfind -package qcheck,qcstm,ppx_deriving.show examples/q.cma examples/q.native

counter:
	ocamlbuild -use-ocamlfind -package qcheck,qcstm,ppx_deriving.show examples/counter.cma examples/counter.native

waterjug:
	ocamlbuild -use-ocamlfind -package qcheck,qcstm,ppx_deriving.show examples/waterjug.cma examples/waterjug.native

stk:
	ocamlbuild -use-ocamlfind -package qcheck,qcstm,ppx_deriving.show examples/stk.cma examples/stk.native

hashtable:
	ocamlbuild -use-ocamlfind -package qcheck,qcstm,ppx_deriving.show examples/hashtable.cma examples/hashtable.native

buf:
	ocamlbuild -use-ocamlfind -package qcheck,qcstm,ppx_deriving.show examples/buf.cma examples/buf.native

putget: putgetstub.so
	ocamlbuild -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,qcstm,ppx_deriving.show examples/putget.native

putgetstub.so: examples/putgetstub.c
	if [[ ! -d _build ]]; then mkdir _build; fi
	if [[ ! -d _build/src ]]; then mkdir _build/examples; fi
	gcc -shared -o _build/examples/putgetstub.so -fPIC examples/putgetstub.c

stdio: stdiostub.o
	ocamlbuild -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,qcstm,ppx_deriving.show -lflags -custom,examples/stdiostub.o examples/stdio.cma examples/stdio.top
	ocamlbuild -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,qcstm,ppx_deriving.show -lflags examples/stdiostub.o examples/stdio.native

stdiostub.o: examples/stdiostub.c
	ocamlbuild examples/stdiostub.o

cq: cqstub.o
	ocamlbuild -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,qcstm,ppx_deriving.show -lflags -custom,examples/cqstub.o examples/cq.cma examples/cq.top
	ocamlbuild -use-ocamlfind -package ctypes,ctypes.foreign,qcheck,qcstm,ppx_deriving.show -lflags examples/cqstub.o examples/cq.native

cqstub.o: examples/cqstub.c
	ocamlbuild examples/cqstub.o

clean:
	ocamlbuild -clean
	rm -rf doc
	rm -f data.dat
