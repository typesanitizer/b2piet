.PHONY: all clean

.DEFAULT: all

.SILENT: headers clean

all: 
	ocamlbuild -use-ocamlfind -cflags '-w -8' src/bf2piet.byte

native:
	ocamlbuild -use-ocamlfind src/bf2piet.native

headers:
	# the directory for saving .cmi should be in sync with .merlin
	for file in src/*.mli ; do \
		echo "$$file" | sed "s/\.mli/\.cmi/" | xargs ocamlbuild; \
	done

clean:
	# deletes _build so no need to separately remove .cmi files
	ocamlbuild -clean

test:
	ocamlbuild -use-ocamlfind src/tests.byte
	./tests.byte

doc:
	ocamlbuild -use-ocamlfind bf2piet.docdir/index.html
