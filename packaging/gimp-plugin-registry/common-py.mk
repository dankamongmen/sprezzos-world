PYDEFAULT := $(shell pyversions -vd)
PYCOMPILE := $(shell which pycompile| grep -v 'not found')

ifndef SCRIPT_SRCDIR
 SCRIPT_SRCDIR := $(CURDIR)
endif

ifdef PYCOMPILE
build: $(SCRIPT:%=$(SCRIPT_SRCDIR)/%c)

$(SCRIPT_SRCDIR)/%c: $(SCRIPT_SRCDIR)/%
	pycompile -V $(PYDEFAULT) $*
else
build:
	#nothing to do without pycompile
endif

install: $(SCRIPT:%=$(DESTDIR)$(PLUGINSCRIPTDIR)/%)

$(DESTDIR)$(PLUGINSCRIPTDIR)/%: $(SCRIPT_SRCDIR)/%
	if ! test -d $(DESTDIR)$(PLUGINSCRIPTDIR); then \
		install -d -m 755 $(DESTDIR)$(PLUGINSCRIPTDIR) ;\
	fi
	install -m 755 $(SCRIPT_SRCDIR)/$* $@

clean:
	rm -f $(SCRIPT_SRCDIR)/*.pyc

.PHONY: build install 
