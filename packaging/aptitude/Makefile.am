MAINTAINERCLEANFILES=Makefile.in aclocal.m4 config.h.in

# Do this the simple way -- it means "make dist" will fail if you
# disable stuff, and has other ugliness, but it is much easier and
# more maintainable than the "correct" alternatives.
SRCDIRS=@SRCDIRS@
DOCDIRS=@DOCDIRS@
TESTDIRS=@TESTDIRS@

SUBDIRS=buildlib $(SRCDIRS) $(DOCDIRS) m4 po $(TESTDIRS)

dist_bin_SCRIPTS = aptitude-create-state-bundle aptitude-run-state-bundle

MANPAGE_LOCALES=gl

TLMANPAGES = $(wildcard $(srcdir)/aptitude.??.8)
TLHELPTXTS = help.txt $(wildcard $(srcdir)/help-??.txt) $(wildcard $(srcdir)/help-??_??.txt)
TLDEFAULTS = $(wildcard $(srcdir)/aptitude-defaults.??) $(wildcard $(srcdir)/aptitude-defaults.??_??)
EXTRA_DIST = config.rpath  ChangeLog.SVN Doxyfile.in aclocal.m4 \
	FAQ README.BOOST README.CWIDGET README.i18n README.INTERFACES README.NAMING \
	README.SMART-POINTERS README.THREADS aptitude-hackers-guide.txt \
	function_groups function_pkgs aptitude-defaults \
	section-descriptions \
	$(TLDEFAULTS) \
	$(TLMANPAGES) $(TLHELPTXTS)

pkgdatadir=$(datadir)/$(PACKAGE)

pkgdata_DATA = $(TLHELPTXTS) COPYING NEWS FAQ \
	function_groups function_pkgs aptitude-defaults section-descriptions \
	$(TLDEFAULTS)

dist_check_SCRIPTS = check_boost.sh

TESTS = check_boost.sh

install-data-local:
	[ -d $(DESTDIR)$(STATEDIR) ] || $(mkinstalldirs) $(DESTDIR)$(STATEDIR)
# FIXME: this really ought to use the autoconf directory variables

	for l in $(MANPAGE_LOCALES); do \
	  $(mkinstalldirs) $(DESTDIR)$(mandir)/$$l/man8; \
	  $(INSTALL_DATA) $(srcdir)/aptitude.$$l.8 $(DESTDIR)$(mandir)/$$l/man8/aptitude.8; \
	done

#uninstall-local:
#rm -fr $(DESTDIR)$(STATEDIR)
# Is -r a good idea here?


ACLOCAL_AMFLAGS = -I m4
