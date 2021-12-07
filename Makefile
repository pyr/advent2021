all: clojure ocaml

.PHONY: ocaml
ocaml:
	@(cd ocaml && make)

clojure:
	@clojure -X runner/run
