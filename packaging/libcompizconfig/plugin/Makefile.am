INCLUDES =			     \
	@COMPIZ_CFLAGS@ \
	-I$(top_srcdir)/include	     \
	-I$(top_srcdir)/src \
	-DPLUGINDIR=\"$(plugindir)\" \
	-DLIBDIR=\"$(libdir)\"

libccp_la_LDFLAGS = -module -avoid-version -no-undefined
libccp_la_LIBADD = $(top_builddir)/src/libcompizconfig.la
libccp_la_SOURCES = ccp.c

moduledir = $(plugindir)

module_LTLIBRARIES = libccp.la

