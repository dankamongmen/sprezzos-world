SUBDIRS = po pixmaps src

install-user: all
	(cd src && $(MAKE) $(AM_MAKEFLAGS) install-user)

uninstall-user:
	(cd src && $(MAKE) $(AM_MAKEFLAGS) uninstall-user)

EXTRA_DIST = merge.patch mkinstalldirs \
	libgimp/gettext.h libgimp/stdplugins-intl.h
