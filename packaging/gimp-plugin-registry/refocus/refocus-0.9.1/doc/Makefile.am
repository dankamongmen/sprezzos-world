## Process this file with automake to produce Makefile.in

EXTRA_DIST = doc.html doc.pdf doc.xml using-inc.xml install-inc.xml images

doc.html tex-math-equations.tex: doc.xml using-inc.xml install-inc.xml
	xsltproc --stringparam tex.math.in.alt latex /usr/share/sgml/docbook/xsl-stylesheets-1.49-1/xhtml/docbook.xsl doc.xml >doc.html
doc.pdf: doc.xml using-inc.xml install-inc.xml
	docbook2pdf doc.xml
help.html: help.xml using-inc.xml
	xsltproc /usr/share/sgml/docbook/xsl-stylesheets-1.49-1/xhtml/docbook.xsl help.xml >help.html
install-no-links.xml: remove-links.xsl install-inc.xml install.xml
	xsltproc remove-links.xsl install.xml >install-no-links.xml
INSTALL.txt: install-no-links.xml
	docbook2txt install-no-links.xml
	-rm INSTALL.txt
	mv install-no-links.txt INSTALL.txt
PNG: tex-math-equations.tex
	latex tex-math-equations.tex
	dvi2bitmap tex-math-equations.dvi
clean:
	-rm tex-math-equations* install-no-links.xml
