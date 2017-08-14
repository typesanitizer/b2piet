.PHONY: all clean metajsonml bytecode native test doc doc-default copycss

.DEFAULT: all

.SILENT: headers

all: metajsonml bytecode

metajsonml:
	atdgen -t src/metaJson.atd
	atdgen -j -j-std src/metaJson.atd

bytecode: metajsonml
	ocamlbuild -use-ocamlfind src/b2piet.byte

native: metajsonml
	ocamlbuild -use-ocamlfind src/b2piet.native

clean:
	rm -f src/metaJson_*
	ocamlbuild -clean

# Use native version as bytecode is *much* slower
test: metajsonml
	ocamlbuild -use-ocamlfind src/tests.native
	./tests.native

doc-default:
	ocamlbuild -use-ocamlfind b2piet.docdir/index.html

copycss:
	cp style/style.css b2piet.docdir/

doc: doc-default copycss
