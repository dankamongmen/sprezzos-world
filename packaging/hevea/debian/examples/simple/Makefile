text.html: text.tex
	hevea text.tex

text.dvi: text.tex
	latex text.tex

text.ps: text.dvi
	dvips -o text.ps text.dvi

clean:
	rm -f *.ps *.dvi *.html *.aux *.haux *log
