include ../../../globalmakeflags

MODULES=$(FRONTEND_MODULES)
INSTALLMODULES=$(addprefix install-,$(MODULES))
CLEANMODULES=$(addprefix clean-,$(MODULES))
SOBJS=$(addsuffix .so,$(FRONTEND_MODULES))

all: $(MODULES)

$(MODULES):
	$(MAKE) -C $@
	cp $@/$@.so .

install: $(INSTALLMODULES)

$(INSTALLMODULES):
	$(MAKE) -C $(patsubst install-%,%,$@) install

clean: $(CLEANMODULES)

$(CLEANMODULES):
	$(MAKE) -C $(patsubst clean-%,%,$@) clean
	-@rm -f $(SOBJS)

XGETTEXT = xgettext
TEMPLATES = \
   $(srcdir)/debian/cdebconf-gtk-udeb.templates \
   $(srcdir)/debian/cdebconf-newt-udeb.templates \
   $(srcdir)/debian/cdebconf-slang-udeb.templates \
   $(srcdir)/debian/cdebconf-text-udeb.templates

templates: $(TEMPLATES)

$(srcdir)/debian/cdebconf-gtk-udeb.templates: gtk/gtk.c
$(srcdir)/debian/cdebconf-newt-udeb.templates: newt/newt.c
$(srcdir)/debian/cdebconf-slang-udeb.templates: slang/slang.c
$(srcdir)/debian/cdebconf-text-udeb.templates: text/text.c

$(TEMPLATES):
	$(XGETTEXT) -c --keyword=get_text:2,3 --omit-header --no-location $? -o - | $(srcdir)/debian/po2templates > $@

.PHONY: all templates install clean $(MODULES) $(INSTALLMODULES) $(CLEANMODULES)
