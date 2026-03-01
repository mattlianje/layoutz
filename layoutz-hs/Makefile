.PHONY: build test repl clean bundle

VERSION := 0.3.1.0
BUNDLE_DIR := bundles

build:
	cabal build

test:
	cabal test --test-show-details=streaming

repl:
	cabal repl

clean:
	cabal clean
	rm -rf $(BUNDLE_DIR)

bundle: build
	@rm -rf $(BUNDLE_DIR) && mkdir -p $(BUNDLE_DIR)
	cabal sdist -o $(BUNDLE_DIR)
	@echo "\nBundle ready: $(BUNDLE_DIR)/"
