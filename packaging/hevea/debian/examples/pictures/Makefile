all: png.html gif.html

png.html: text.tex
	hevea -o png.html text
	imagen png

gif.html: text.tex
	hevea gif.hva -o gif.html text
	imagen -gif gif

text.ps: text.dvi
	dvips -o text.ps text.dvi

text.dvi: text.tex
	latex text

clean:
	rm -f *.dvi *.ps *.log *.image.tex *.png *.gif *.html *.aux
