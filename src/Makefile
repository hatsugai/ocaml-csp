OCAML_INCLUDE=-I `ocamlc -where`
OCAML_OPTIONS=-bin-annot

%.cmi:	%.mli
	ocamlc $(OCAML_INCLUDE) $(OCAML_OPTIONS) -c $<

%.cmx:	%.ml
	ocamlopt $(OCAML_INCLUDE) $(OCAML_OPTIONS) -c $<

%.cmo:	%.ml
	ocamlc $(OCAML_INCLUDE) $(OCAML_OPTIONS) -c $<

all:	csp.cmo csp.cmx

csp.cmo:	csp.ml csp.cmi
csp.cmx:	csp.ml csp.cmi

clean:
	rm *.cmi *.cmo *.cmx *.cmt *.cmti *.cma *.annot *.o
