dnl Process this file with autoconf to produce a configure script.
dnl The ONLY thing this is used for is to configure for different
dnl linux architectures and configurations, it is not used to make the
dnl code more portable

AC_INIT
AC_CONFIG_SRCDIR([configure.ac])
CFLAGS="-Wall $CFLAGS -D_GNU_SOURCE"
AC_CONFIG_AUX_DIR([.])
AC_CONFIG_HEADER([src/config.h:src/config.h.in])

AC_DEFINE([DEBCONF_VERSION], [2.0])
AC_DEFINE([PACKAGE], ["cdebconf"])

dnl Checks for programs.
AC_PROG_MAKE_SET
AC_PROG_CC
AC_ISC_POSIX

PKG_PROG_PKG_CONFIG

dnl We must have sed...
AC_PROG_SED

dnl Use pod2man for generating manpages
AC_PATH_PROG([POD2MAN], [pod2man])
AS_IF([test -n "$POD2MAN"], [], [AC_MSG_ERROR([pod2man is missing; cannot continue!])])
AC_SUBST([POD2MAN])

dnl setup source and build paths
TOPDIR=$(dirname $0)
if test "$TOPDIR" = "${TOPDIR#/}"; then
	# not absolute, resolve it 
	TOPDIR=$(readlink -f $(dirname $(pwd)/$0))
fi
AC_SUBST([TOPDIR])
BUILDDIR=$(pwd)
AC_SUBST([BUILDDIR])

if test "$TOPDIR" != "$BUILDDIR"; then
	echo "Setting up build directory"
	for d in `(cd $TOPDIR; find . -type d -a -not -name CVS -a -not -name debian)`; do
		mkdir -p $BUILDDIR/$d
		test -f $TOPDIR/$d/Makefile && cp $TOPDIR/$d/Makefile $BUILDDIR/$d/
		test -f $TOPDIR/$d/modules.mak && cp $TOPDIR/$d/modules.mak $BUILDDIR/$d/
	done
fi

dnl Enable debugging?
AC_ARG_WITH([debug], [AS_HELP_STRING([--without-debug], [turn off debugging?])])
if test "$with_debug" != "no"; then
  AC_DEFINE([DODEBUG])
  CFLAGS="$CFLAGS -g -D_DEBUG_"
fi

dnl Use rpath? (for testing mostly)
AC_ARG_WITH([rpath], [AS_HELP_STRING([--with-rpath], [use rpath? (for testing)])])
if test "$with_rpath" != "no"; then
  RPATH="-Wl,-rpath,$(pwd)/src"
else
  RPATH="-Wl,-rpath,$prefix/lib/cdebconf"
fi
AC_SUBST([RPATH])

dnl What db modules to build?
AC_ARG_WITH([db], [AS_HELP_STRING([--with-db], [db modules to build])])
DB_MODULES=$with_db

dnl What frontend modules to build?
AC_ARG_WITH([frontend], [AS_HELP_STRING([--with-frontend], [frontend modules to build])])
FRONTEND_MODULES=$with_frontend

dnl debconf config file
DEBCONFCONFIG="/etc/cdebconf.conf"
AC_ARG_WITH([conffile], [AS_HELP_STRING([--with-conffile=PATH], [cdebconf config file [/etc/debconf.conf]])],
[case "$withval" in
  "") AC_MSG_ERROR([invalid conffile specified]) ;;
  *) DEBCONFCONFIG="$withval" ;;
 esac])
AC_DEFINE_UNQUOTED([DEBCONFCONFIG], ["$DEBCONFCONFIG"])

DEFAULT_FRONTEND=text
AC_ARG_WITH([default-frontend], [AS_HELP_STRING([--with-default-frontend], [default frontend to use [text]])],
[case "$withval" in
  "") AC_MSG_ERROR([invalid default frontend specified]) ;;
  *)  DEFAULT_FRONTEND="$withval";;
 esac])
AC_SUBST([DEFAULT_FRONTEND])

dnl whether to use libtextwrap
AC_ARG_WITH([textwrap], [AS_HELP_STRING([--with-textwrap], [use libtextwrap for line-folding])],
[if test "x$withval" = "xyes" ; then
   AC_CHECK_LIB([textwrap], [textwrap_init],
      [AC_DEFINE([HAVE_LIBTEXTWRAP],[1])
         TEXTWRAP_LIBS="-ltextwrap"])
 fi])
AC_SUBST([TEXTWRAP_LIBS])

dnl logging to syslog (should be configurable in config file
AC_ARG_WITH([syslog-logging], [AS_HELP_STRING([--with-syslog-logging], [enable logging to syslog])],
[if test "x$withval" = "xyes"; then
   AC_DEFINE([SYSLOG_LOGGING])
 fi])

dnl compile cdebconf udeb?
AC_ARG_ENABLE([d_i], [AS_HELP_STRING([--enable-d-i], [build cdebconf for the debian-installer])],
[test "$enableval" = "yes" && CFLAGS="$CFLAGS -DDI_UDEB"])

dnl Check for libraries
PKG_CHECK_MODULES([DI],[libdebian-installer])
AC_CHECK_LIB([dl], [dlopen])

dnl Autodetect what to build if not specified

dnl Databases - which to include.
if test -z "$DB_MODULES"; then
    DB_MODULES="textdb rfc822db stack"

    AC_CHECK_LIB([mysqlclient], [mysql_init], [DB_MODULES="$DB_MODULES mysql"], [AC_MSG_WARN([cannot build mysql database])])
fi

dnl Frontends - which to include.

if test -z "$FRONTEND_MODULES"; then
    FRONTEND_MODULES="passthrough text"

    AC_CHECK_LIB([bogl], [bowl_flush], [FRONTEND_MODULES="$FRONTEND_MODULES bogl"], [AC_MSG_WARN([cannot build BOGL frontend])])
    PKG_CHECK_EXISTS([glib-2.0 gtk+-x11-2.0 >= 2.24 pango gdk-pixbuf-2.0], [FRONTEND_MODULES="$FRONTEND_MODULES gtk"], [AC_MSG_WARN([cannot build GTK frontend])])
    PKG_CHECK_EXISTS([ncurses], [FRONTEND_MODULES="$FRONTEND_MODULES ncurses"], [AC_MSG_WARN([cannot build ncurses frontend])])
    PKG_CHECK_EXISTS([libnewt slang], [FRONTEND_MODULES="$FRONTEND_MODULES newt"], [AC_MSG_WARN([cannot build Newt frontend])])
    PKG_CHECK_EXISTS([slang], [FRONTEND_MODULES="$FRONTEND_MODULES slang"], [AC_MSG_WARN([cannot build Slang frontend])])
fi

for frontend in $FRONTEND_MODULES; do
    case $frontend in
      gtk) PKG_CHECK_MODULES([GTK_X11],[glib-2.0 gtk+-x11-2.0 >= 2.24 pango gdk-pixbuf-2.0]);;
      ncurses) PKG_CHECK_MODULES([NCURSES],[ncurses]);;
      newt) PKG_CHECK_MODULES([NEWT],[libnewt slang]);;
      slang) PKG_CHECK_MODULES([SLANG],[slang]);;
      *);;
    esac
done

AC_SUBST([FRONTEND_MODULES])
AC_SUBST([DB_MODULES])

dnl Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([fcntl.h sys/ioctl.h unistd.h])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_MODE_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_HEADER_TIME

dnl Checks for library functions.
AC_PROG_GCC_TRADITIONAL
AC_TYPE_SIGNAL
AC_CHECK_FUNCS([socket strdup strstr strtol])

AC_CONFIG_FILES([globalmakeflags src/cdebconf.conf-dist
 src/Makefile
 man/Makefile
 src/modules/frontend/gtk/Makefile
 src/modules/frontend/ncurses/Makefile
 src/modules/frontend/newt/Makefile
 src/modules/frontend/slang/Makefile
 src/modules/frontend/text/Makefile
])
AC_OUTPUT
