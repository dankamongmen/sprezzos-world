context.html: context.tex context.hbbl
	hevea -fix context.tex

context.hbbl: context.tex rewriting.bib
	hevea context
	bibhva context

rewriting.bib: rewriting.bib.gz
	gunzip -c rewriting.bib.gz > rewriting.bib

context.ps: context.dvi
	dvips -o context.ps context.dvi

context.dvi: context.tex context.bbl
	latex context
	bibtex context
	latex context
	-(grep -q "Rerun" context.log && latex context)
	-(grep -q "Rerun" context.log && latex context)

clean:
	rm -f *.ps *.dvi *.html *.aux *.haux *log *.blg *.bbl *.hbbl
