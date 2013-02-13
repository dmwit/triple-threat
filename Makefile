GRAPHS := $(patsubst %.dot,%.eps,$(wildcard graphs/*.dot))
TEX := pdflatex -interaction=nonstopmode

examples.pdf: examples.tex $(GRAPHS)
	$(TEX) examples

graphs/%.eps: graphs/%.dot
	dot -Teps -o$@ $^
