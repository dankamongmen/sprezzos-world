Index: lens-3.7.1.2/tests/doctests.hs
===================================================================
--- lens-3.7.1.2.orig/tests/doctests.hs	2013-01-01 22:22:52.236614171 +0000
+++ lens-3.7.1.2/tests/doctests.hs	2013-01-01 22:23:24.524573289 +0000
@@ -11,9 +11,9 @@
 main :: IO ()
 main = getSources >>= \sources -> doctest $
     "-isrc"
-  : "-idist/build/autogen"
+  : "-idist-ghc/build/autogen"
   : "-optP-include"
-  : "-optPdist/build/autogen/cabal_macros.h"
+  : "-optPdist-ghc/build/autogen/cabal_macros.h"
   : "-hide-all-packages"
   : map ("-package="++) deps ++ sources
 
