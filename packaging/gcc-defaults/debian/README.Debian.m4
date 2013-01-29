divert(-1)
define(`ifenabled', `ifelse(index(PACKAGES, `$1'), -1, `dnl', `$2')')

divert`'dnl
dnl --------------------------------------------------------------------------
		The Debian GNU Compiler Collection Setup
		========================================

Abstract
--------

Debian uses a default version of GCC for most packages; however, some
packages require another version.  So, Debian allows several versions
of GCC to coexist on the same system, and selects the default version
by means of the gcc-defaults package, which creates symbolic links as
appropriate.

Versions of GCC present in Debian Wheezy
-----------------------------------------

- GCC 4.7 is the default compiler for Go and Java on all architectures.
  On x86 architectures it is the default compiler for C, C++, Objective-C,
  Objective-C++ and Fortran 95.

- GCC 4.6 is the default compiler for Ada and D (language version 2).
  On non x86 architectures it is the default compiler for C, C++,
  Objective-C, Objective-C++ and Fortran 95.

- GCC 4.5 was removed for the release of Wheezy.

- GCC 4.4 is the default compiler for D (language version 1).

- GCC 4.1 was removed for the release of Wheezy (was the default for Pascal).

- GCC 3.4 was removed for the release of Squeeze.

- GCC 3.3 is not provided anymore; it is used to build libstdc++5 on
  the amd64 and i386 architectures. It is expected that libstdc++5 is
  not available anymore for the release following Lenny.

- GCC 2.95 and GCC 2.7.2.3 were removed for the release of Lenny.

How are the default compilers selected?
---------------------------------------

Starting in Debian 3.0, there is now a gcc-defaults package set. This
creates the actual packages for gcc, gnat, g++, gobjc, chill, gcj, gij,
gdc and gpc.  These packages will depend on the corresponding default
compiler for that architecture. For Debian 5.0 for example, "gcc"
depends on "gcc-4.4", which means that the "gcc-4.4" package will
install a binary called "gcc-4.4", which is symlinked to in the "gcc"
package as "gcc".

This may seem confusing, but what it allows you do to is install more
than one version of the GCC compiler collection at the same time,
making sure you are always using the one preferred for that
architecture. To use the other compiler, simply set CC=gcc-4.6, or
similar.

The default compiler versions for Debian GNU/OS_NAME on DEB_ARCH are
(minor version numbers omitted):

	cpp		: cpp-PV_CPP
	gcc		: gcc-PV_GCC
ifenabled(`g++',`	g++		: g++-PV_GPP')
ifenabled(`gfortran',`	gfortran	: gfortran-PV_GFORT')
ifenabled(`gcj',`	gcj		: gcj-PV_GCJ')
ifenabled(`gcj',`	gij		: gij-PV_GIJ')
ifenabled(`gccgo',`	gccgo		: gccgo-PV_GGO')
ifenabled(`gobjc',`	gobjc		: gobjc-PV_GOBJC')
ifenabled(`gobjc++',`	gobjc++		: gobjc++-PV_GOBJCXX')
ifenabled(`gnat',`	gnat		: gnat-PV_GNAT')
ifenabled(`gpc',`	gpc		: gpc-PV_GPC')
ifenabled(`gdc',`	gdc		: gdc-PV_GDC')
ifenabled(`gcc-spu',`	gcc-spu		: gcc-PV_SPU-spu')
ifenabled(`g++-spu',`	g++-spu		: g++-PV_SPU-spu')
ifenabled(`gfortran-spu',`	gfortran-spu	: gfortran-PV_SPU-spu')

ifdef(`GFDL',`dnl
Documentation for the default compilers can be found in

	/usr/share/doc/<compiler>-<package version>.
',`dnl
Most of the documentation for GCC including the manual pages is
licensed under the GFDL and therefore not included in the main section.
')`'dnl

Thanks to gcc-defaults, each architecture can choose its own preferred
compiler for each language, and that preference can change without
requiring a complete rebuild of both compiler packages for all
architectures.


Practical implications
----------------------

The most important practical implications are in the merging/linking
of object files built with different compilers; If you use the 4.1
C compiler, you should use the `gcc-4.1' compiler driver for all your
work.  When configuring sources, use

    CC=gcc-4.4 ./configure <configure options> 	# bash
    setenv CC gcc-4.4; ./configure <options>	# csh

When calling make, use `make CC=gcc-4.4'.


C Application Binary Interface
------------------------------

Starting with Debian 4.0 (lenny), gcc-4.1 and newer compilers do
support the `long double' datatype with 128bit on the alpha, powerpc,
s390 and sparc architectures.  Libraries and applications using this
datatype have to be rebuilt using the compiler versions in Lenny
unless these depend on libc6/libc6.1 and libstdc++6, which still have
compatibility with a 64bit `long double' datatype.


gcc/g++/... are not handled using alternatives
----------------------------------------------

The symlinks in /usr/bin (gcc, g++, ...) are not handled using the Debian
alternative mechanism. There are differences in the architecture specific
ABI on some architectures and the C++ ABI differs as well. Having the
symlinks managed by alternatives doesn't allow reliable builds with the
same major/minor version of the compiler. To use another compiler version,
set the appropriate environment variables as described above in the section
"Practical implications".


C++ libraries
-------------

To use the libstdc++ library for debugging (found in the
libstd++6-<GCC version>-dbg package), add /usr/lib/debug to your
LD_LIBRARY_PATH. For gdb to display the source you need to get the
correspondig gcc-X.Y source package, unpack the source and point gdb
to the location of the source (`dir' directive).


C++ Application Binary Interface
--------------------------------

Sometimes, the C++ ABI of GCC changes.  It is impossible to link
object files that obey different ABIs into an executable.  When the
ABI changes, Debian provides a new version of libstdc++ with a new
soname.

Version 4 of the ABI was used by GCC 3.0 and 3.1; it is no longer
supported.

Version 5 of the ABI is common to GCC 3.2 and 3.3; GCC 3.3
provides libstdc++5. It is only supported as a runtime library.

Version 6 of the ABI is common to GCC 3.4 and later; GCC 4.3 provides
libstdc++6.


Bugs
----

Before submitting a bug, please read README.Bugs in this directory.


Feedback appreciated
--------------------

Feedback about this document is appreciated; preferably as a Severity:
wishlist bug against Package: gcc . For general discussions and
questions, subscribe and/or email the debian-gcc@lists.debian.org mailing
list.


Maintainers of these packages
-----------------------------

Matthias Klose <doko@debian.org>
Ray Dassen <jdassen@debian.org>
Philip Blundell <pb@debian.org>			(arm-linux)
Jeff Bailey <jbailey@nisa.net>			(hurd-i386)
Joel Baker <fenton@debian.org>			(netbsd-i386)
Ben Collins <bcollins@debian.org>		(sparc-linux)
Falk Hueffner <falk@debian.org>			(alpha-linux)
Randolph Chung <tausq@debian.org>		(ia64-linux, hppa-linux)
Thiemo Seufer <ths@networkno.de>		(mips*-linux)
Dan Jacobowitz <dan@debian.org>			(powerpc-linux)
Gerhard Tonn <GerhardTonn@swol.de>		(s390-linux)
Roman Zippel <zippel@linux-m68k.org>		(m68k-linux)
Ludovic Brenta <ludovic.brenta@insalien.org>	(gnat)
Arthur Loiret <arthur.loiret@gmail.com>		(gdc)

===============================================================================

