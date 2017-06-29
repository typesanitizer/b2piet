.PHONY: all clean

.DEFAULT: all

all: 
	ocamlbuild -use-ocamlfind src/bf2piet.byte

headers:
	# the directory for saving .cmi should be in sync with .merlin
	for file in src/*.mli ; do \
		echo "$$file" | sed "s/\.mli/\.cmi/" | xargs ocamlbuild; \
	done

clean:
	# deletes _build so no need to separately remove .cmi files
	ocamlbuild -clean
