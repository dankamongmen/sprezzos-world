# Process this file with autoconf to produce a configure script.
# Copyright © 2003-2005 Scott James Remnant <scott@netsplit.com>

AC_PREREQ(2.57)
AC_INIT([build-essential], 11.1)
AC_CONFIG_SRCDIR([list2depends])

AM_INIT_AUTOMAKE([1.7.7 foreign])
AM_MAINTAINER_MODE

AC_CHECK_PROG([have_dpkgarchitecture], [dpkg-architecture], yes)
if test "x$have_dpkgarchitecture" != "xyes"
then
	AC_MSG_ERROR([[The dpkg development files (dpkg-dev) must be installed to build this package.]])
fi

AC_MSG_CHECKING([[for Debian architecture]])
DEB_HOST_ARCH=`dpkg-architecture -qDEB_HOST_ARCH`
AC_MSG_RESULT($DEB_HOST_ARCH)
AC_SUBST(DEB_HOST_ARCH)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
