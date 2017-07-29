native-code: color.ml color.mli
	ocamlbuild color.native

byte-code: color.ml color.mli
	ocamlbuild color.byte

clean:
	rm ./*.cm*
	rm ./*.a
	rm ./*.o

install: native-code byte-code
	#cd _build && ocamlmklib color.ml
	ocamlfind install color META _build/*.cmi _build/*.cmo _build/*.cmx _build/*.cma _build/*.cmxa

uninstall:
	ocamlfind remove color
