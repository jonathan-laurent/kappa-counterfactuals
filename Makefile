all:
	dune build bin/resimulate.exe

clean:
	dune clean
	rm -f tests/*.json tests/input* tests/stats

test: all
	cd tests ; KaSim test.ka -var VA 10000 -trace t.json
	dune exec bin/resimulate.exe -- tests/t.json --block-instances b --stats tests/stats --silent
	cat tests/stats