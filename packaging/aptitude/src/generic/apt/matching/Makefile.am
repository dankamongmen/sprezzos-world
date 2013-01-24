MAINTAINERCLEANFILES = Makefile.in

localedir = $(datadir)/locale
INCLUDES = -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
LDADD = @LIBINTL@

noinst_LIBRARIES = libgeneric-matching.a

libgeneric_matching_a_SOURCES = \
	compare_patterns.cc	\
	compare_patterns.h	\
	match.cc                \
	match.h                 \
	parse.cc		\
	parse.h			\
	pattern.cc		\
	pattern.h		\
	serialize.cc		\
	serialize.h