SOURCES = mondrian.ml ui.ml
LIBS = graphics.cma

# Crée l'exécutable
mondrian: mondrian.ml
	ocamlc -o mondrian $(LIBS) mondrian.ml

ui: ui.ml
	ocamlc -o ui $(LIBS) $(SOURCES)

# Efface les fichiers auxiliaires
clean:
	$(RM) *~ *.cmi *.cmo
