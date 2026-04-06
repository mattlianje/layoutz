.PHONY: build test repl clean demo tui-demo showcase fmt fmt-check

build:
	clj -M -e "(compile 'layoutz.core)"

test:
	clj -M:test

repl:
	clj

clean:
	rm -rf .cpcache classes target

demo:
	clojure -M:demo

tui-demo:
	clojure -M:tui-demo

showcase:
	clojure -M:showcase

fmt:
	clj -M:fmt

fmt-check:
	clj -M:fmt-check
