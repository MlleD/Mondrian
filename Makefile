# Crée l'exécutable
mondrian: mondrian.ml
	ocamlc -o mondrian mondrian.ml

# Efface les fichiers auxiliaires
clean:
	rm *.cmi *.cmo
