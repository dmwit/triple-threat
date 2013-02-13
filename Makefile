GRAPHS := $(patsubst %.dot,%.png,$(wildcard graphs/*.dot))
TEX := pdflatex -interaction=nonstopmode

examples.pdf: examples.tex $(GRAPHS)
	$(TEX) examples

graphs/%.png: graphs/%.dot
	dot -Tpng -o$@ $^
