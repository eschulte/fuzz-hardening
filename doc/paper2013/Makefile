all: paper2013.pdf

%.pdf : %.tex
	latex $< 
	latex $< 
	bibtex $(<:%.tex=%)
	latex --output-format=pdf $< 
#	evince $(<:%.tex=%.pdf)

%.eps : %.svg
	inkscape --export-eps=$(<:%.svg=%.eps) $<

clean:
	rm -f *.dvi *.pdf *.log *.aux *.blg *.bbl
