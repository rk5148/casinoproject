.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play: build
	OCAMLRUNPARAM=b dune exec src/casino.exe

check:
	@bash check.sh

finalcheck:
	@bash check.sh final

zip:
	rm -f casino.zip
	zip -r casino.zip . -x@exclude.lst

clean:
	dune clean
	rm -f casino.zip

doc:
	dune build @doc
