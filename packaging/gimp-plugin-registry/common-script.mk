ifndef SCRIPT_SRCDIR
 SCRIPT_SRCDIR := $(CURDIR)
endif

build:

install: $(SCRIPT:%=$(DESTDIR)$(PLUGINSCRIPTDIR)/%)

$(DESTDIR)$(PLUGINSCRIPTDIR)/%: $(SCRIPT_SRCDIR)/%
	if ! test -d $(DESTDIR)$(PLUGINSCRIPTDIR); then \
		install -d -m 755 $(DESTDIR)$(PLUGINSCRIPTDIR) ;\
	fi
	install -m 644 $(SCRIPT_SRCDIR)/$* $@

.PHONY: build install 
