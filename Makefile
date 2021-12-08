all: clojure ocaml

.PHONY: ocaml
ocaml:
	@(cd ocaml && make)

.PHONY: clojure
clojure:
	@clojure -X runner/run
