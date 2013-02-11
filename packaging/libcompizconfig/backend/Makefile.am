## Process this file with automake to produce Makefile.in

INCLUDES =			     \
	@COMPIZ_CFLAGS@ \
	-I$(top_srcdir)/include	     \
	-I$(top_srcdir)/src \
	-DPLUGINDIR=\"$(PLUGINDIR)\" \
	-DLIBDIR=\"$(libdir)\"

libini_la_LDFLAGS = -module -avoid-version -no-undefined $(all_libraries)
libini_la_LIBADD  = $(top_builddir)/src/libcompizconfig.la
libini_la_SOURCES = ini.c
backenddir = $(libdir)/compizconfig/backends

METASOURCES = AUTO

backend_LTLIBRARIES = libini.la

