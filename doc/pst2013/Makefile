all: pst2013.pdf

%.pdf : %.tex
	latex $< 
	latex $< 
	bibtex $(<:%.tex=%)
	latex --output-format=pdf $< 

clean:
	rm -f *.dvi *.pdf *.log *.aux *.blg *.bbl
