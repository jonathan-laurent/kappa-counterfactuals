BIN=resimulate

all:
	ocamlbuild -use-ocamlfind main.native
	mv main.native $(BIN)

clean:
	rm -rf _build $(BIN)
	rm -f tests/*.json tests/input*

test: all
	cd tests ; KaSim test.ka -var VA 10000 -trace t.json
	./resimulate tests/t.json --block-instances b --stats stats --silent
	cat stats