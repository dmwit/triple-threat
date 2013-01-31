GRAPHS := $(patsubst %.dot,%.eps,$(wildcard graphs/*.dot))
TEX := pdflatex -halt-on-error

all: locks.pdf graphs.pdf

locks.pdf: locks.tex
	$(TEX) locks

graphs.pdf: graphs.tex $(GRAPHS)
	$(TEX) graphs
	echo $^

graphs/%.eps: graphs/%.dot
	dot -Teps -o$@ $^

.PHONY: all
