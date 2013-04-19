
doc_name = sunroof

doc_tex  = $(doc_name).tex
doc_pdf  = $(doc_name).pdf
doc_bbl  = $(doc_name).bbl
doc_blg  = $(doc_name).blg
doc_log  = $(doc_name).log
doc_aux  = $(doc_name).aux
doc_ptb  = $(doc_name).ptb

all:	bib pdf

bib:
	pdflatex $(doc_tex)
	bibtex $(doc_name)
	pdflatex $(doc_tex)

lhs:
	lhs2TeX -o $(doc_tex) $(doc_lhs)

pdf:
	pdflatex $(doc_tex)
	# rm $(doc_tex)

clean:
	rm -f $(doc_log) $(doc_aux) $(doc_bbl) $(doc_blg) $(doc_ptb)

clean-build: clean
	rm -f $(doc_pdf)
	