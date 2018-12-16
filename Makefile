run: sat_solver.cmo arbre_bsp.cmo mondrian.cmo
	ocamlc -o run graphics.cma sat_solver.cmo arbre_bsp.cmo mondrian.cmo

arbre_bsp.cmo: arbre_bsp.ml arbre_bsp.cmi
	ocamlc -c arbre_bsp.ml

arbre_bsp.cmi: arbre_bsp.mli
	ocamlc arbre_bsp.mli

mondrian.cmo: mondrian.ml mondrian.cmi
	ocamlc -c mondrian.ml

mondrian.cmi: mondrian.mli
	ocamlc mondrian.mli

sat_solver.cmo: sat_solver.ml sat_solver.cmi
	ocamlc -c sat_solver.ml

sat_solver.cmi: sat_solver.mli
	ocamlc sat_solver.mli

clean:
	rm *.cmi *.cmo
