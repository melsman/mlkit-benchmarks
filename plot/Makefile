PDFLATEX=pdflatex -halt-on-error

all: plotGengc.pdf plotGengcA.pdf plotGengcB.pdf plotMlton.pdf plotMltonA.pdf plotMltonB.pdf

%.pdf: %.tex
	$(PDFLATEX) $<

.PHONY: clean
clean:
	rm -rf *~ *.pdf *.aux *.log
