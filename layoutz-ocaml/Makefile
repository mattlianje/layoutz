.PHONY: build test retest clean repl fmt demo

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
