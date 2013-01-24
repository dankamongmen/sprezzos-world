INCLUDES=-I$(top_builddir) -I$(top_srcdir) -I$(top_srcdir)/src -I$(top_srcdir)/intl

SUBDIRS = \
	toplevel \
	util \
	view-impls

noinst_LIBRARIES=libgtk.a

pkgdatadir=$(datadir)/$(PACKAGE)

pkgdata_DATA = aptitude.glade

EXTRA_DIST = aptitude.glade

libgtk_a_SOURCES=\
	areas.cc \
	areas.h \
	changelog.cc \
	changelog.h \
	constants.h \
	dashboardtab.cc \
	dashboardtab.h \
	dependency_chains_tab.cc \
	dependency_chains_tab.h \
	download.cc \
	download.h \
	dpkg_terminal.cc \
	dpkg_terminal.h \
	entitysummary.cc \
	entitysummary.h \
	entityview.cc \
	entityview.h \
	errortab.cc \
	errortab.h \
	filesview.cc \
	filesview.h \
	globals.cc \
	globals.h \
	gui.cc \
	gui.h \
	hyperlink.cc \
	hyperlink.h \
	info.cc \
	info.h \
	init.cc \
	init.h \
	mainwindow.cc \
	mainwindow.h \
	notify.cc \
	notify.h \
	packageinformation.cc \
	packageinformation.h \
	packagestab.cc \
	packagestab.h \
	pkgview.cc \
	pkgview.h \
	post_event.cc \
	post_event.h \
	previewtab.cc \
	previewtab.h \
	progress.cc \
	progress.h \
	resolver.cc \
	resolver.h \
	screenshot.cc \
	screenshot.h \
	screenshot_cache.cc \
	screenshot_cache.h \
	tab.cc \
	tab.h \
	treeview_cell_tooltips.cc \
	treeview_cell_tooltips.h
