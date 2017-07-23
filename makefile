.PHONY: all clean bytecode native headers test doc doc-default

.DEFAULT: all

.SILENT: headers

all: bytecode native

bytecode:
	ocamlbuild -use-ocamlfind src/bf2piet.byte

native:
	ocamlbuild -use-ocamlfind src/bf2piet.native

headers:
	# the directory for saving .cmi should be in sync with .merlin
	for file in src/*.mli ; do \
		echo "$$file" | sed "s/\.mli/\.cmi/" | xargs ocamlbuild; \
	done

clean:
	ocamlbuild -clean

test:
	ocamlbuild -use-ocamlfind src/tests.byte
	./tests.byte

doc-default:
	ocamlbuild -use-ocamlfind bf2piet.docdir/index.html

doc: doc-default
	cp style/style.css _build/bf2piet.docdir/
