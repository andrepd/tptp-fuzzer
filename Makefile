.PHONY: tptp-fuzzer
tptp-fuzzer:
	ocamlbuild -use-ocamlfind main.native
	mv main.native tptp-fuzzer

.PHONY: clean
clean:
	ocamlbuild -clean
