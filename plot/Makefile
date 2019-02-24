PDFLATEX=pdflatex -halt-on-error

all: plotGengc.pdf plotMlton.pdf

%.pdf: %.tex
	$(PDFLATEX) $<

.PHONY: clean
clean:
	rm -rf *~ *.pdf *.aux *.log
