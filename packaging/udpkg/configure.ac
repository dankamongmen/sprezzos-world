
dnl Process this file with autoconf to produce a configure script.
dnl The ONLY thing this is used for is to configure for different
dnl linux architectures and configurations, it is not used to make the
dnl code more portable

AC_INIT([udpkg], m4_esyscmd(dpkg-parsechangelog | perl -ne 'print $1 if m/^Version: (.*)$/;'))
AC_CONFIG_SRCDIR([udpkg.c])
CFLAGS="$CFLAGS -Wall -Wwrite-strings -Wmissing-prototypes"
AC_CONFIG_AUX_DIR(.)
AC_CONFIG_HEADER(config.h:config.h.in)

dnl Checks for programs.
AC_PROG_CC
AC_ISC_POSIX

dnl Checks for libraries.

dnl Enable debugging?
AC_ARG_WITH(debug,[  --without-debug         turn off debugging?])
if test "$with_debug" != "no"; then
  AC_DEFINE([DODEBUG], [], [Should we do debugging?])
  CFLAGS="$CFLAGS -g"
fi

dnl Do dependency checking?
AC_ARG_WITH(depends,[  --without-depends       do not do dependency checking])
if test "$with_depends" != "no"; then
  AC_DEFINE([DODEPENDS], [], [Should we do full dependency checking?])
fi

dnl Load templates?
AC_ARG_WITH(loadtemplate,[  --without-loadtemplate  do not call debconf-loadtemplate])
if test "$with_loadtemplate" != "no"; then
  AC_DEFINE([DOLOADTEMPLATE], [], [Should we call debconf-loadtemplate?])
fi

dnl Support localized status files?
AC_ARG_WITH(l10n,[  --without-l10n          do not support localized status files])
if test "$with_l10n" != "no"; then
  AC_DEFINE([SUPPORTL10N], [], [Support l10n?])
fi

dnl Do we need --remove support?
AC_ARG_WITH(remove,[  --without-remove        do not support the --remove flag])
if test "$with_remove" != "no"; then
  AC_DEFINE([DOREMOVE], [], [Should we support the --remove flag?])
fi

dnl dpkg library directory
ADMINDIR="/var/lib/dpkg"
AC_ARG_WITH(admindir,[  --with-admindir=DIR     store dpkg data in DIR [/var/lib/dpkg]],
[case "$withval" in
  "") AC_MSG_ERROR(invalid admindir specified) ;;
  *) ADMINDIR="$withval" ;;
 esac])
AC_SUBST(ADMINDIR) 

dnl use busybox?
AC_ARG_WITH(busybox,[  --with-busybox=DIR      link with busybox in DIR [/usr/src/busybox]])

if test -n "$with_busybox"; then
  BUSYBOXDIR="/usr/src/busybox"
  if test "$with_busybox" != "yes"; then
    BUSYBOXDIR="$with_busybox"
  fi
  AC_MSG_CHECKING(for busybox in $BUSYBOXDIR)
  if test -f "$BUSYBOXDIR/busybox.def.h"; then 
    AC_MSG_RESULT(yes)
  else 
    AC_MSG_RESULT(no)
    exit 1
  fi
fi
AC_SUBST(BUSYBOXDIR)

dnl Checks for header files.
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS(fcntl.h string.h sys/time.h unistd.h)

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_HEADER_TIME

dnl Checks for library functions.
AC_TYPE_SIGNAL

AC_OUTPUT(makefile:makefile.in)
