INCLUDES = \
	-I$(top_srcdir)/include

noinst_LTLIBRARIES = libsystem.la

libsystem_la_SOURCES = \
	devfs.c \
	dpkg.c \
	packages.c \
	prebaseconfig.c \
	utils.c

EXTRA_libsystem_la_SOURCES = \
	subarch-generic.c \
	subarch-arm-linux.c \
	subarch-armeb-linux.c \
	subarch-armel-linux.c \
	subarch-armhf-linux.c \
	subarch-m68k-linux.c \
	subarch-mips-linux.c \
	subarch-mipsel-linux.c \
	subarch-powerpc-linux.c \
	subarch-sh4-linux.c \
	subarch-sparc-linux.c \
	subarch-x86-linux.c \
	subarch-x86-kfreebsd.c

libsystem_la_LIBADD = \
	@SUBARCH_SYSTEM@

libsystem_la_DEPENDENCIES = \
	@SUBARCH_SYSTEM@
