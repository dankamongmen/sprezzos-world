AC_INIT([mklibs],m4_esyscmd(dpkg-parsechangelog | perl -ne 'print $1 if m/^Version: (.*)$/;'))
AM_INIT_AUTOMAKE([foreign no-define])
AC_CONFIG_HEADERS([config.h])
AM_MAINTAINER_MODE

AC_CANONICAL_TARGET

AC_PROG_CXX

AC_CONFIG_FILES([
Makefile
lib/Makefile
lib/mklibs/Makefile
lib/mklibs/utils/Makefile
src/Makefile
src/mklibs-readelf/Makefile
])

AC_OUTPUT
