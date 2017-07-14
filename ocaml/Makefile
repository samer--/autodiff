
# The library requires the delimcc package 
#
# The typical command to use the OCaml top-level:
# $ ocaml 

# $Id$

INCLUDES=
OCAMLC=ocamlfind ocamlc -g -package batteries,delimcc -linkpkg
OCAMLOPT=ocamlfind ocamlopt -g -package batteries,delimcc -linkpkg
OCAMLDOC=ocamlfind ocamldoc -g -package batteries,delimcc -linkpkg
OCAML=rlwrap ocaml

OBJECTS=utils.cmo
SOURCES=utils.ml 

.SUFFIXES: .ml .mli .cmi .cmo .pdf

%.cmi: %.ml
	$(OCAMLC) -c $(INCLUDES) $<

%.cmo: %.ml 
	$(OCAMLC) -c $(INCLUDES) $<

utils.cmo: utils.ml
adarrow.cmo: dist.ml utils.cmi

clean::
	  rm -f *.cm[iox] *.[oa] *~ 


