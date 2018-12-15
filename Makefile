run: arbre_bsp.cmo mondrian.cmo
	ocamlc -o run graphics.cma arbre_bsp.cmo mondrian.cmo

arbre_bsp.cmo: arbre_bsp.ml arbre_bsp.cmi
	ocamlc -c arbre_bsp.ml

arbre_bsp.cmi: arbre_bsp.mli
	ocamlc arbre_bsp.mli

mondrian.cmo: mondrian.ml mondrian.cmi
	ocamlc -c mondrian.ml

mondrian.cmi: mondrian.mli
	ocamlc mondrian.mli

clean:
	rm *.cmi *.cmo
