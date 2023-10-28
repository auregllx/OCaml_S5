dpll: functions.ml
	ocamlfind ocamlopt -o func -package str -linkpkg functions.ml

clean:
	rm -f *.cmi *.cmx *.o func
