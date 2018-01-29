BIN=resimulate

all:
	ocamlbuild -use-ocamlfind main.native
	mv main.native $(BIN)

clean:
	rm -rf _build $(BIN)
	rm -f tests/*.json tests/input*

test: all
	cd tests ; KaSim test.ka -var VA 100 -trace t.json 
	./resimulate -b b tests/t.json --stats stats
	cat stats