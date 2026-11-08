.PHONY: build test bench repl clean bundle raymarcher

VERSION := 0.3.3.0
BUNDLE_DIR := bundles

build:
	cabal build

test:
	cabal test --test-show-details=streaming

# layoutz next to Bubble Tea and Brick (see bench/Main.hs). Bubble Tea needs
# Go 1.21+: the newest ~/sdk/go*/bin/go if there is one, else `go` on PATH
# (or pick one with GO=/path/to/go). Brick is its own cabal project in
# bench/brick. Whichever doesn't build is left out of the table. Each run
# rewrites bench/results.svg and the results section of bench/BENCHES.md.
GO ?= $(or $(shell ls $(HOME)/sdk/go*/bin/go 2>/dev/null | sort -V | tail -n 1),go)
BENCH_BIN := $(CURDIR)/dist-newstyle/bench

bench:
	@mkdir -p $(BENCH_BIN); others=""; \
	if (cd bench/bubbletea && $(GO) build -o $(BENCH_BIN)/bubbletea-dashboard .); then \
		others="$$others --bubbletea $(BENCH_BIN)/bubbletea-dashboard"; \
	else echo "Could not build bench/bubbletea, leaving Bubble Tea out."; fi; \
	if (cd bench/brick && cabal build -v0 && cp "$$(cabal list-bin brick-dashboard)" $(BENCH_BIN)/brick-dashboard); then \
		others="$$others --brick $(BENCH_BIN)/brick-dashboard"; \
	else echo "Could not build bench/brick, leaving Brick out."; fi; \
	cabal bench layoutz-bench --benchmark-options="$$others --out $(CURDIR)/bench"

repl:
	cabal repl

raymarcher:
	cabal run raymarcher

clean:
	cabal clean
	rm -rf $(BUNDLE_DIR)

bundle: build
	@rm -rf $(BUNDLE_DIR) && mkdir -p $(BUNDLE_DIR)
	cabal sdist -o $(BUNDLE_DIR)
	@echo "\nBundle ready: $(BUNDLE_DIR)/"
