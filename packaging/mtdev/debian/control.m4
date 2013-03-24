divert(-1)

define(`checkdef',`ifdef($1, , `errprint(`error: undefined macro $1
')m4exit(1)')')
define(`errexit',`errprint(`error: undefined macro `$1'
')m4exit(1)')

define(`PN', `$1')
ifdef(`PRI', `', `
    define(`PRI', `$1')
')
define(`MAINTAINER', `Nobuhiro Iwamatsu <iwamatsu@debian.org>')

define(`ifenabled', `ifelse(index(enabled_pkgs, `$1'), -1, `dnl', `$2')')

divert`'dnl
dnl --------------------------------------------------------------------------
Source: mtdev
Section: libs
Priority: optional
ifelse(DIST,`Ubuntu',`dnl
Maintainer: Ubuntu Developers <ubuntu-devel-discuss@lists.ubuntu.com>
XSBC-Original-Maintainer: MAINTAINER
', `dnl 
Maintainer: MAINTAINER
')dnl DIST
Build-Depends: debhelper (>= 7.0.0),
               pkg-config,
               quilt,
               dpkg-dev (>= 1.14.17),
               automake,
               libtool,
               cdbs
Standards-Version: 3.9.1
Homepage: http://bitmath.org/code/mtdev/

Package: libmtdev1
Architecture: any
Depends: ${shlibs:Depends},
         ${misc:Depends}
Description: Multitouch Protocol Translation Library - shared library
 libmtdev is a library for translating evdev multitouch events using the legacy
 protocol to the new multitouch slots protocol. This is necessary for kernel
 drivers that have not been updated to use the newer protocol.

Package: libmtdev-dev
Section: libdevel
Architecture: any
Depends: libmtdev1 (= ${binary:Version}),
         ${misc:Depends}
Description: Multitouch Protocol Translation Library - dev files
 libmtdev is a library for translating evdev multitouch events using the legacy
 protocol to the new multitouch slots protocol. This is necessary for kernel
 drivers that have not been updated to use the newer protocol.
 .
 This package contains files that are needed to build applications.

Package: mtdev-tools
Section: libdevel
Architecture: any
Depends: libmtdev1 (= ${binary:Version}),
         ${shlibs:Depends},
         ${misc:Depends}
Description: Multitouch Protocol Translation Library - test tools
 libmtdev is a library for translating evdev multitouch events using the legacy
 protocol to the new multitouch slots protocol. This is necessary for kernel
 drivers that have not been updated to use the newer protocol.
 .
 This package provides some test tools for the libmtdev library.
  + mtdev-test: prints the information comming from the kernel
ifenabled(`mtdevudev',`
Package: libmtdev1-udeb
Section: debian-installer
Architecture: any
XC-Package-Type: udeb
Depends: ${shlibs:Depends},
         ${misc:Depends}
Description: Multitouch Protocol Translation Library - shared library
 libmtdev is a library for translating evdev multitouch events using the legacy
 protocol to the new multitouch slots protocol. This is necessary for kernel
 drivers that have not been updated to use the newer protocol.
 .
 This is a udeb, or a microdeb, for the debian-installer.
')`'dnl
