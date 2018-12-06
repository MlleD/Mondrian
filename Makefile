# Crée l'exécutable
mondrian: mondrian.ml
	ocamlc -o mondrian -c mondrian.ml -I Graphics

# Efface les fichiers auxiliaires
clean:
	rm *.cmi *.cmo
