SOURCES = bsp.ml ui.ml game.ml
LIBS = graphics.cma

# Crée l'exécutable
game: $(SOURCES)
	ocamlc -o game $(LIBS) $(SOURCES)

# Efface les fichiers auxiliaires
clean:
	$(RM) *~ *.cmi *.cmo
