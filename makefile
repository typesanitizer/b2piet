.PHONY: all clean metajsonml bytecode native test doc doc-default copycss

.DEFAULT: all

.SILENT: headers

all: metajsonml bytecode

metajsonml:
	atdgen -t src/metaJson.atd
	atdgen -j -j-std src/metaJson.atd

bytecode: metajsonml
	ocamlbuild -use-ocamlfind src/bf2piet.byte

native: metajsonml
	ocamlbuild -use-ocamlfind src/bf2piet.native

clean:
	rm -f src/metaJson_*
	ocamlbuild -clean

test: metajsonml
	ocamlbuild -use-ocamlfind src/tests.byte
	./tests.byte

doc-default:
	ocamlbuild -use-ocamlfind bf2piet.docdir/index.html

copycss:
	cp style/style.css _build/bf2piet.docdir/

doc: doc-default copycss
