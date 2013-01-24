MAINTAINERCLEANFILES = Makefile.in

SUBDIRS = history matching

localedir = $(datadir)/locale
INCLUDES = -I$(top_builddir) -I$(srcdir) -I$(top_srcdir) -I$(top_srcdir)/src
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@
LDADD = @LIBINTL@

EXTRA_DIST = README.hier

noinst_LIBRARIES = libgeneric-apt.a
noinst_PROGRAMS  = pkg_hier_dump

libgeneric_apt_a_SOURCES = \
        aptcache.cc         \
        aptcache.h          \
        apt.cc              \
        apt.h               \
        aptitudepolicy.cc   \
        aptitudepolicy.h    \
        aptitude_resolver.cc \
        aptitude_resolver.h \
        aptitude_resolver_cost_settings.cc \
        aptitude_resolver_cost_settings.h \
        aptitude_resolver_cost_syntax.cc \
        aptitude_resolver_cost_syntax.h \
	aptitude_resolver_cost_types.cc \
	aptitude_resolver_cost_types.h \
        aptitude_resolver_universe.cc \
        aptitude_resolver_universe.h \
        apt_undo_group.cc   \
        apt_undo_group.h    \
	changelog_parse.cc  \
	changelog_parse.h   \
        config_signal.cc    \
        config_signal.h     \
	desc_parse.cc       \
	desc_parse.h        \
	download_manager.cc \
	download_manager.h  \
	download_install_manager.cc \
	download_install_manager.h  \
	download_queue.cc   \
	download_queue.h    \
	download_update_manager.cc  \
	download_update_manager.h   \
	download_signal_log.cc \
	download_signal_log.h  \
	dpkg.cc             \
	dpkg.h              \
	dump_packages.cc    \
	dump_packages.h     \
	globals.cc          \
        infer_reason.cc     \
        infer_reason.h      \
	log.cc		    \
	log.h		    \
	parse_dpkg_status.cc\
	parse_dpkg_status.h \
        pkg_acqfile.cc      \
        pkg_acqfile.h       \
        pkg_changelog.cc    \
        pkg_changelog.h     \
        pkg_hier.cc         \
        pkg_hier.h          \
        resolver_manager.cc \
        resolver_manager.h  \
        rev_dep_iterator.h  \
	screenshot.cc       \
	screenshot.h        \
        tags.cc             \
        tags.h              \
        tasks.cc            \
        tasks.h

pkg_hier_dump_SOURCES = pkg_hier_dump.cc
pkg_hier_dump_LDADD = $(top_builddir)/src/generic/util/libgeneric-util.a libgeneric-apt.a
