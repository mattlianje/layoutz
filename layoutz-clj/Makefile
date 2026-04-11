.PHONY: build test repl clean demo tui-demo inline-demo showcase fmt fmt-check

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
