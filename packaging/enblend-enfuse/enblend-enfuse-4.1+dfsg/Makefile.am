EXTRA_DIST = \
        README.txt VERSION \
        CMakeLists.txt CMakeModules ConfigureChecks.cmake config.h.cmake

if BUILD_DOC
  MAYBE_DOC = doc
endif

SUBDIRS = include src $(MAYBE_DOC)
DIST_SUBDIRS = include src doc

ACLOCAL_AMFLAGS = -I m4

# created by configure(1)
DISTCLEANFILES = config-h.texi


.PHONY: xhtml
xhtml:
	$(MAKE) --directory=doc $@

.PHONY: install-xhtml
install-xhtml:
	$(MAKE) --directory=doc $@

.PHONY: uninstall-xhtml
uninstall-xhtml:
	$(MAKE) --directory=doc $@

.PHONY: validate-xhtml
validate-xhtml:
	$(MAKE) --directory=doc $@
