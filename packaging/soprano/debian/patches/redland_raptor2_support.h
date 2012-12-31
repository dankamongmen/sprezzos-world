From: Luigi Toscano <luigi.toscano@tiscali.it>
From: Modestas Vainius <modax@debian.org>
Subject: Add support for raptor V2 to the redland backend
Forwarded: no
Origin: other
Last-Update: 2011-06-23

This patch should be considered as pretty dirty FTBFS fix.

--- a/backends/redland/redlandbackend.cpp
+++ b/backends/redland/redlandbackend.cpp
@@ -24,6 +24,7 @@
 
 #include "redlandbackend.h"
 
+#include <raptor2/raptor.h>
 #include <redland.h>
 
 #include "redlandworld.h"
--- a/backends/redland/redlandmodel.h
+++ b/backends/redland/redlandmodel.h
@@ -28,6 +28,7 @@
 #include <QtCore/QtGlobal>
 #include <QtCore/QTextStream>
 
+#include <raptor2/raptor.h>
 #include <redland.h>
 
 #include "storagemodel.h"
--- a/backends/redland/redlandnodeiteratorbackend.h
+++ b/backends/redland/redlandnodeiteratorbackend.h
@@ -24,6 +24,7 @@
 
 #include "iteratorbackend.h"
 
+#include <raptor2/raptor.h>
 #include <redland.h>
 
 #include <QtCore/QSharedDataPointer>
--- a/backends/redland/redlandqueryresult.h
+++ b/backends/redland/redlandqueryresult.h
@@ -23,6 +23,7 @@
 #ifndef SOPRANO_BACKEND_REDLAND_QUERY_RESULT_H
 #define SOPRANO_BACKEND_REDLAND_QUERY_RESULT_H
 
+#include <raptor2/raptor.h>
 #include <redland.h>
 #include <QString>
 
--- a/backends/redland/redlandstatementiterator.h
+++ b/backends/redland/redlandstatementiterator.h
@@ -23,6 +23,7 @@
 #ifndef SOPRANO_BACKEND_REDLAND_STATEMENT_ITERATOR_H
 #define SOPRANO_BACKEND_REDLAND_STATEMENT_ITERATOR_H
 
+#include <raptor2/raptor.h>
 #include <redland.h>
 
 #include "iteratorbackend.h"
--- a/backends/redland/redlandworld.h
+++ b/backends/redland/redlandworld.h
@@ -23,6 +23,7 @@
 #ifndef SOPRANO_BACKEND_REDLAND_WORLD_H
 #define SOPRANO_BACKEND_REDLAND_WORLD_H
 
+#include <raptor2/raptor.h>
 #include <redland.h>
 
 #include "error.h"
--- a/cmake/modules/FindRedland.cmake
+++ b/cmake/modules/FindRedland.cmake
@@ -44,6 +44,7 @@ if(REDLAND_CONFIG_EXECUTABLE)
     execute_process(
       COMMAND ${REDLAND_CONFIG_EXECUTABLE} --cflags
       OUTPUT_VARIABLE redland_LIBS_ARGS)
+    set(REDLAND_CFLAGS ${redland_LIBS_ARGS})
     string( REPLACE " " ";" redland_LIBS_ARGS ${redland_LIBS_ARGS} )
     foreach( _ARG ${redland_LIBS_ARGS} )
       if(${_ARG} MATCHES "^-I")
