.PHONY: all clean depends metajsonml bytecode native test doc doc-default copycss

.DEFAULT: metajsonml bytecode

.SILENT: headers

all: metajsonml bytecode native

metajsonml:
	atdgen -t src/metaJson.atd
	atdgen -j -j-std src/metaJson.atd

bytecode: metajsonml
	ocamlbuild -use-ocamlfind src/b2piet.byte

native: metajsonml
	ocamlbuild -use-ocamlfind src/b2piet.native

depends:
	opam pin add --yes --no-action b2piet .
	opam install --yes --unset-root --deps-only b2piet
	opam pin remove b2piet

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
