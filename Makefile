# adn: adn.ml
# 	ocamlfind ocamlopt -o adn -package str -linkpkg adn.ml

# expressions: expressions.ml
# 	ocamlfind ocamlopt -o expressions -package str -linkpkg expressions.ml

# clean:
# 	rm -f *.cmi *.cmx *.o adn expressions

all: adn expressions

SOURCES = adn.ml expressions.ml

EXECUTABLES = adn expressions

%.cmo: %.ml
	ocamlc -c $<

adn: adn.cmo
	ocamlc -o $@ $^

expressions: expressions.cmo
	ocamlc -o $@ $^

clean:
	rm -f *.cmo *.cmi $(EXECUTABLES)


