
SRCDIR = ./src


PREFIX = /usr
MANDIR = $(PREFIX)/share/man
INFODIR = $(PREFIX)/share/info
CONFFLAGS += --prefix=$(PREFIX) --mandir=$(MANDIR) --infodir=$(INFODIR) \
             --bindir=`/usr/bin/gimptool-2.0 --gimpplugindir`/plug-ins

DEB_HOST_GNU_TYPE   ?= $(shell dpkg-architecture -qDEB_HOST_GNU_TYPE)
DEB_BUILD_GNU_TYPE  ?= $(shell dpkg-architecture -qDEB_BUILD_GNU_TYPE)
ifeq ($(DEB_BUILD_GNU_TYPE), $(DEB_HOST_GNU_TYPE))
  CONFFLAGS += --build $(DEB_HOST_GNU_TYPE)
else
  CONFFLAGS += --build $(DEB_BUILD_GNU_TYPE) --host $(DEB_HOST_GNU_TYPE)
endif

LDFLAGS += -Wl,-z,defs

$(SRCDIR)/config.status: $(SRCDIR)/configure
ifneq "$(wildcard /usr/share/misc/config.sub)" ""
	cp -f /usr/share/misc/config.sub $(SRCDIR)/config.sub
endif
ifneq "$(wildcard /usr/share/misc/config.guess)" ""
	cp -f /usr/share/misc/config.guess $(SRCDIR)/config.guess
endif

	$(PRE-CONFIGURE-HOOK)
	cd $(SRCDIR); GIMPTOOL="/usr/bin/gimptool-2.0" \
	              CFLAGS="$(CFLAGS) $(EXTRA_CFLAGS)" \
	              LDFLAGS="$(LDFLAGS) $(EXTRA_LDFLAGS)" \
	              CPPFLAGS="$(CPPFLAGS) $(EXTRA_CPPFLAGS)" ./configure $(CONFFLAGS)
	$(POST-CONFIGURE-HOOK)

build: build-stamp

build-stamp: $(SRCDIR)/config.status
	$(PRE-BUILD-HOOK)
	make -C $(SRCDIR) MAKEFLAGS=
	$(POST-BUILD-HOOK)
	touch $@

install: build-stamp
	$(PRE-INSTALL-HOOK)
	make -C $(SRCDIR) install DESTDIR=$(DESTDIR)
	$(POST-INSTALL-HOOK)

clean:
	rm -f build-stamp
	cd $(SRCDIR); [ ! -r Makefile ] || make distclean
	rm -f $(SRCDIR)/config.sub $(SRCDIR)/config.guess
	$(CLEAN-HOOK)

.PHONY: install clean build

