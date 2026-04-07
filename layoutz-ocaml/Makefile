.PHONY: build test retest clean repl fmt demo showcase install deps

deps:
	opam install . --deps-only --with-test --yes

build:
	dune build

test:
	dune test

retest:
	dune test --force

clean:
	dune clean

repl:
	dune utop lib

fmt:
	dune fmt

demo:
	dune exec ./demo.exe

showcase:
	dune exec ./showcase_app.exe

install: deps
	dune build @install
	dune install
