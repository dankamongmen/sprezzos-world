MAINTAINERCLEANFILES=Makefile.in

localedir = $(datadir)/locale
INCLUDES = -I$(top_builddir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
LDADD = @LIBINTL@
SWEEPERHELPTXTS = mine-help.txt $(wildcard $(srcdir)/mine-help-??.txt) \
  $(wildcard $(srcdir)/mine-help-??_??.txt)

noinst_LIBRARIES = libcmine.a

libcmine_a_SOURCES=	\
	cmine.cc	\
	cmine.h		\
	board.cc	\
	board.h

pkgdatadir=$(datadir)/$(PACKAGE)

pkgdata_DATA = $(SWEEPERHELPTXTS)
EXTRA_DIST = $(SWEEPERHELPTXTS)

