AUTOMAKE_OPTIONS = foreign no-dependencies

AM_CFLAGS = -DPACKAGE_DATA_DIR=\"$(pkgdatadir)\"

electricsheep_LDADD = -lavformat -lavcodec -lavutil -lm -lz

bin_PROGRAMS = electricsheep electricsheep-preferences

pkgdata_DATA = electricsheep-wait.avi electricsheep-preferences.glade

electricsheep_preferences_CFLAGS = @libglade_CFLAGS@ -DPACKAGE_DATA_DIR=\"$(pkgdatadir)\"
electricsheep_preferences_LDFLAGS = @libglade_LIBS@ -export-dynamic
electricsheep_preferences_SOURCES = electricsheep-preferences.c utils.c md5.c


electricsheep_SOURCES = electricsheep.c getdate.c utils.c md5.c
man1_MANS = electricsheep.man
EXTRA_DIST = $(man1_MANS) $(pkgdata_DATA) electricsheep.man.in md5.h \
	electricsheep.xml.in electricsheep.spec.in mkinstalldirs getdate.h \
	electricsheep-preferences.glade electricsheep.h electricsheep-saver \
	16_electricsheep 

GNOME_SCREENSAVER_DATADIR = $(shell pkg-config --variable themesdir gnome-screensaver)
GNOME_SCREENSAVER_PRIVEXEDIR = $(shell pkg-config --variable privlibexecdir gnome-screensaver)

install-data-local:
	-$(INSTALL) electricsheep.desktop $(GNOME_SCREENSAVER_DATADIR)
	-$(INSTALL) electricsheep-saver $(GNOME_SCREENSAVER_PRIVEXEDIR)
	-$(INSTALL) -d $(pkgdatadir)
	-$(INSTALL) 16_electricsheep /usr/share/gconf/defaults
	-update-gconf-defaults

uninstall-local:
	-rm /usr/share/gconf/defaults/16_electricsheep
	-rm -rf $(pkgdatadir)
	-rm $(GNOME_SCREENSAVER_PRIVEXEDIR)/electricsheep-saver
	-rm $(GNOME_SCREENSAVER_DATADIR)/electricsheep.desktop
	-update-gconf-defaults

