SUBDIRS = system

INCLUDES = \
	-I$(top_srcdir)/include

lib_LTLIBRARIES = libdebian-installer.la libdebian-installer-extra.la

libdebian_installer_la_SOURCES = \
	exec.c \
	hash.c \
	log.c \
	mem.c \
	mem_chunk.c \
	package.c \
	package_parser.c \
	packages.c \
	packages_parser.c \
	parser.c \
	parser_rfc822.c \
	slist.c \
	string.c \
	tree.c \
	utils.c

libdebian_installer_extra_la_SOURCES = \
	list.c \
	release.c

libdebian_installer_la_LIBADD = \
	system/libsystem.la

libdebian_installer_extra_la_LIBADD = \
	libdebian-installer.la

libdebian_installer_la_LDFLAGS = \
	-version-number $(LIBRARY_VERSION_LIBTOOL) \
	-Wl,--version-script=$(srcdir)/libdebian-installer.ver \
	-Wl,--no-undefined-version

libdebian_installer_extra_la_LDFLAGS = \
	-version-number $(LIBRARY_VERSION_LIBTOOL) \
	-Wl,--version-script=$(srcdir)/libdebian-installer-extra.ver \
	-Wl,--no-undefined-version

libdebian_installer_la_DEPENDENCIES = \
	$(srcdir)/libdebian-installer.ver \
	system/libsystem.la

libdebian_installer_extra_la_DEPENDENCIES = \
	$(srcdir)/libdebian-installer-extra.ver \
	libdebian-installer.la

