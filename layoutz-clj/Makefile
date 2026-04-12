.PHONY: build test repl clean demo tui-demo inline-demo showcase fmt fmt-check jar deploy release

build:
	clj -M -e "(compile 'layoutz.core)"

test:
	clj -M:test

repl:
	clj -M -e "(require '[layoutz.core :refer :all])" -r

clean:
	rm -rf .cpcache classes target

demo:
	@clojure -M:demo

tui-demo:
	clojure -M:tui-demo

inline-demo:
	clojure -M:inline-demo

showcase:
	clojure -M:showcase

fmt:
	clj -M:fmt

fmt-check:
	clj -M:fmt-check

jar:
	clj -T:build jar

deploy:
	clj -T:build deploy

# Bump version in build.clj, then: make release
release:
	clj -T:build deploy
	$(eval V := $(shell grep '(def version' build.clj | sed 's/.*"\(.*\)".*/\1/'))
	git tag -a "clj-v$(V)" -m "layoutz-clj $(V)"
	@echo "Released $(V). Run 'git push origin clj-v$(V)' to push the tag."
