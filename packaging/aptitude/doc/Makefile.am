SUBDIRS=cs de en es fi fr it ja pl po4a

EXTRA_DIST=aptitude-common.xsl aptitude-html.xsl aptitude-man.xsl aptitude-txt.xsl aptitude.css html-to-text xx/Makefile.am

PO4AFLAGS = --srcdir=$(top_srcdir)/doc --destdir=$(top_builddir)/doc \
	--package-name='$(PACKAGE)' --package-version='$(PACKAGE_VERSION)' \
	--msgid-bugs-address='$(PACKAGE_BUGREPORT)' \
	$(top_srcdir)/doc/po4a/po4a.cfg

if HAVE_PO4A

dist: update-po

update-po:
	$(PO4A) --no-backups --force --no-translations $(PO4AFLAGS)

po4a:
	$(PO4A) --no-backups $(PO4AFLAGS)

endif

.PHONY: update-po
