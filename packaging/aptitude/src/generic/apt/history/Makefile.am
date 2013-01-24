MAINTAINERCLEANFILES = Makefile.in

localedir = $(datadir)/locale
INCLUDES = -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
LDADD = @LIBINTL@

noinst_LIBRARIES = libgeneric-history.a

libgeneric_history_a_SOURCES =	\
	history_entry.cc	\
	history_entry.h