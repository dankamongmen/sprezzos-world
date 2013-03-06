# $Id: Makefile,v 1.5 2004/04/04 00:14:59 karl Exp karl $
# Makefile for tds, by Karl Berry.
# Public domain.

default all: tds.info tds.html tds.dvi tds.pdf

tds.dvi: tds.tex tdsguide.cls
	latex '\nonstopmode \input tds'

tds.pdf: tds.tex tdsguide.cls
	pdflatex '\nonstopmode \input tds'

tds.texi: tds.texi.tmp tds.sed
	sed -f tds.sed $< >$@

tds.texi.tmp: tds.tex tds2texi.el 
	emacs -q --no-site -batch -l tds2texi.el -f tds2texi-convert --eval '(write-file "$@")'

tds.info: tds.texi
	makeinfo --no-split $<

tds_toc.html: tds.texi /usr/local/gnu/bin/texi2html
	texi2html -split_node -menu $<

tds.html: tds.texi
	makeinfo --number-sections --html --no-split $<

check:
	dw <tds.tex
#	chkdelim <tds.tex
	ispell -t -l <tds.tex | sort -u

# so that when we're mirrored on CTAN, we don't show up as a good place
# to get these packages.
packages.zip: Ulogo.fd fancyhdr.sty mflogo.sty url.sty
	zip $@ $^

dist: all packages.zip
#	-rcsdiff -c tds.tex >/tmp/diff
	rm -f tds.texi* *~ 
	rm -f tds.cp tds.fn tds.ky tds.pg tds.tp tds.vr tds.log tds.toc
	tar cvvzf /tmp/tds.tar.gz C* M* tds* index.html packages.zip
#	shar -z tds.dvi >/tmp/shar
#	@echo "Now ci -l -m'version' tds.tex and increment the tdsVersion."
