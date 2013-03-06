DESTDIR = /usr/share/doc/tex-common
TDS_VERSION = 1.1

CLEANFILES = Debian-TeX-Policy.txt Debian-TeX-Policy.html Debian-TeX-Policy.tpt TeX-on-Debian.txt TeX-on-Debian.html TeX-on-Debian.tpt

all: policy-txt policy-html policy-pdf tex-on-debian-txt tex-on-debian-html tex-on-debian-pdf

policy-txt: Debian-TeX-Policy.txt
policy-pdf: Debian-TeX-Policy.pdf
policy-html: Debian-TeX-Policy.html

Debian-TeX-Policy.txt: Debian-TeX-Policy.sgml version.ent
	debiandoc2text Debian-TeX-Policy.sgml

Debian-TeX-Policy.html: Debian-TeX-Policy.sgml version.ent
	-rm -r $@
	debiandoc2html Debian-TeX-Policy.sgml

Debian-TeX-Policy.pdf: Debian-TeX-Policy.sgml version.ent texify-tex-output
	debiandoc2latexpdf -s ./texify-tex-output Debian-TeX-Policy.sgml

tex-on-debian-txt: TeX-on-Debian.txt
tex-on-debian-html: TeX-on-Debian.html
tex-on-debian-pdf: TeX-on-Debian.pdf

TeX-on-Debian.txt: TeX-on-Debian.sgml
	debiandoc2text TeX-on-Debian.sgml

TeX-on-Debian.html: TeX-on-Debian.sgml
	debiandoc2html TeX-on-Debian.sgml

TeX-on-Debian.pdf: TeX-on-Debian.sgml texify-tex-output
	debiandoc2latexpdf -s ./texify-tex-output TeX-on-Debian.sgml

get-tds:
	wget ftp://www.tug.org/tex/tds.tar.gz
	tar -xzf tds.tar.gz
	rm tds.tar.gz

tds-all:
	cd tds-$(TDS_VERSION); make tds.pdf
	cd tds-$(TDS_VERSION); make tds.dvi
	cd tds-$(TDS_VERSION); make tds.html

install: 
	install -d $(DESTDIR)
	cp Debian-TeX-Policy.txt $(DESTDIR)/
	cp Debian-TeX-Policy.pdf $(DESTDIR)/
	cp -R Debian-TeX-Policy.html $(DESTDIR)/
	cp TeX-on-Debian.txt $(DESTDIR)/
	cp TeX-on-Debian.pdf $(DESTDIR)/
	cp -R TeX-on-Debian.html $(DESTDIR)/
	ln -s TeX-on-Debian.txt $(DESTDIR)/README.Debian.txt
	ln -s TeX-on-Debian.pdf $(DESTDIR)/README.Debian.pdf
	# replace the link with a redirect html document, so that
	# the links within the file actually work
	cp README.Debian.html $(DESTDIR)/
	#ln -s TeX-on-Debian.html/index.html $(DESTDIR)/README.Debian.html
	cp obsolete-tex-files.txt $(DESTDIR)/

	cp tds-$(TDS_VERSION)/tds.pdf $(DESTDIR)/
	cp tds-$(TDS_VERSION)/tds.dvi $(DESTDIR)/
	cp tds-$(TDS_VERSION)/tds.html $(DESTDIR)/

clean: tds-clean
	-rm -r $(CLEANFILES)

tds-clean:
	-rm tds-$(TDS_VERSION)/tds.texi*

allclean: clean
	-rm  Debian-TeX-Policy.pdf
	-rm  TeX-on-Debian.pdf

.PHONY: all install clean allclean \
        policy-txt policy-html policy-pdf \
        tex-on-debian-txt tex-on-debian-html tex-on-debian-pdf
