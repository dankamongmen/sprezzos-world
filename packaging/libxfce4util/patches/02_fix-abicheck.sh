Description: fix abicheck.sh for ppc64
Author: Aurelien Jarno <aurel32@debian.org>
Bug: https://bugzilla.xfce.org/show_bug.cgi?id=9146
Bug-Debian: http://bugs.debian.org/682414

Index: libxfce4util-4.10.0/libxfce4util/abicheck.sh
===================================================================
--- libxfce4util-4.10.0.orig/libxfce4util/abicheck.sh	2012-04-28 21:29:28.000000000 +0200
+++ libxfce4util-4.10.0/libxfce4util/abicheck.sh	2012-07-22 20:30:20.444917571 +0200
@@ -20,5 +20,5 @@
 #
 
 cpp -P -DINCLUDE_INTERNAL_SYMBOLS -DINCLUDE_VARIABLES -DALL_FILES ${srcdir:-.}/libxfce4util.symbols | sed -e '/^$/d' -e 's/ G_GNUC.*$//' -e 's/ PRIVATE//' | sort > expected-abi
-nm -D .libs/libxfce4util.so | grep " T\|R\|G " | cut -d ' ' -f 3 | grep -v '^_.*' | grep -v '^ *$' | sort > actual-abi
+nm -D .libs/libxfce4util.so | grep " T\|R\|G\|D " | cut -d ' ' -f 3 | grep -v '^_.*' | grep -v '^ *$' | sort > actual-abi
 diff -u expected-abi actual-abi && rm expected-abi actual-abi
