.PHONY: tptp-fuzzer
tptp-fuzzer:
	ocamlbuild -use-ocamlfind main.native
	mv main.native tptp-fuzzer

clean:
	ocamlbuild -clean
