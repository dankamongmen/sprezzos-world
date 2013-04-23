## Process this file with automake to produce Makefile.in

if ENABLE_GTK_DOC
GTK_DOC_DIR = gtk-doc
else
GTK_DOC_DIR = 
endif


SUBDIRS= src lib doc $(GTK_DOC_DIR)

install-bin:
	cd src; make install-bin
install-admin-bin:
	cd src; make install-admin-bin

if HAVE_ATLAS
dist-hook:
	@echo "**** Don't run make dist when system is configured with atlas."
	@echo "**** Rename lib-atlas, rm config.cache and re-run configure."
	@false
else
dist-hook:
endif
