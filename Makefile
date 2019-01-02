SOURCES = bsp.ml ui.ml game.ml
LIBS = graphics.cma

# Crée l'exécutable
mondrian: $(SOURCES)
	ocamlc -o mondrian $(LIBS) $(SOURCES)

# Efface les fichiers auxiliaires
clean:
	$(RM) *~ *.cmi *.cmo
