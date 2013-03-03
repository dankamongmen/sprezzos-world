Adjust the texmfcnf.lua file to realities in Debian
---
 texmfcnf.lua |   34 +++++++++++++++++-----------------
 1 file changed, 17 insertions(+), 17 deletions(-)

Index: texlive-base-2012.20120529/texmf/web2c/texmfcnf.lua
===================================================================
--- texlive-base-2012.20120529.orig/texmf/web2c/texmfcnf.lua	2012-05-13 08:34:52.000000000 +0900
+++ texlive-base-2012.20120529/texmf/web2c/texmfcnf.lua	2012-06-01 20:43:57.189689613 +0900
@@ -3,17 +3,15 @@
 -- ConTeXt needs a properly expanded TEXMFLOCAL, so here is a
 -- bit of lua code to make that happen
 
-local texmflocal = resolvers.prefixes.selfautoparent();
-texmflocal = string.gsub(texmflocal, "20%d%d$", "texmf-local");
-
 return {
 
     type    = "configuration",
     version = "1.1.0",
-    date    = "2012-05-24",
+    date    = "2012-05-14", -- or so
     time    = "12:12:12",
     comment = "ConTeXt MkIV configuration file",
     author  = "Hans Hagen, PRAGMA-ADE, Hasselt NL",
+    -- adaption by Preining Norbert for the Debian system
 
     content = {
 
@@ -44,14 +42,14 @@
 
             -- only used for FONTCONFIG_PATH & TEXMFCACHE in TeX Live
 
-            TEXMFSYSVAR     = "selfautoparent:texmf-var",
-            TEXMFVAR        = "home:.texlive2012/texmf-var",
+            TEXMFSYSVAR     = "/var/lib/texmf",
+            TEXMFVAR        = "home:.texmf-var",
 
             -- We have only one cache path but there can be more. The first writable one
             -- will be chosen but there can be more readable paths.
 
             TEXMFCACHE      = "$TEXMFSYSVAR;$TEXMFVAR",
-            TEXMFCONFIG     = "home:.texlive2012/texmf-config",
+            TEXMFCONFIG     = "home:.texmf-config",
 
             -- I don't like this texmf under home and texmf-home would make more
             -- sense. One never knows what installers put under texmf anywhere and
@@ -61,14 +59,16 @@
             -- By using prefixes we don't get expanded paths in the cache __path__
             -- entry. This makes the tex root relocatable.
 
-            TEXMFOS         = "selfautodir:",
-            TEXMFMAIN       = "selfautoparent:texmf",
-            TEXMFDIST       = "selfautoparent:texmf-dist",
-
-            TEXMFLOCAL      = texmflocal,
-            TEXMFSYSCONFIG  = "selfautoparent:texmf-config",
-            TEXMFFONTS      = "selfautoparent:texmf-fonts",
-            TEXMFPROJECT    = "selfautoparent:texmf-project",
+            -- TEXMFOS         = "selfautodir:",
+            -- TEXMFSYSTEM     = "selfautoparent:$SELFAUTOSYSTEM",
+            TEXMFMAIN       = "/usr/share/texlive/texmf",
+	    TEXMFDIST       = "/usr/share/texlive/texmf-dist",
+            TEXMFCONTEXT    = "/usr/share/texmf",
+
+            TEXMFLOCAL      = "/usr/local/share/texmf",
+            TEXMFSYSCONFIG  = "/etc/texmf",
+            -- TEXMFFONTS      = "selfautoparent:texmf-fonts",
+            -- TEXMFPROJECT    = "selfautoparent:texmf-project",
 
             TEXMFHOME       = "home:texmf",
          -- TEXMFHOME       = os.name == "macosx" and "home:Library/texmf" or "home:texmf",
@@ -76,7 +76,7 @@
             -- We need texmfos for a few rare files but as I have a few more bin trees
             -- a hack is needed. Maybe other users also have texmf-platform-new trees.
 
-            TEXMF           = "{$TEXMFCONFIG,$TEXMFHOME,!!$TEXMFSYSCONFIG,!!$TEXMFSYSVAR,!!$TEXMFPROJECT,!!$TEXMFFONTS,!!TEXMFMAIN,!!$TEXMFLOCAL,!!$TEXMFDIST}",
+            TEXMF           = "{$TEXMFCONFIG,$TEXMFVAR,$TEXMFHOME,$TEXMFSYSCONFIG,!!$TEXMFSYSVAR,!!$TEXMFLOCAL,!!$TEXMFCONTEXT,!!$TEXMFMAIN,!!$TEXMFDIST}",
 
             TEXFONTMAPS     = ".;$TEXMF/fonts/data//;$TEXMF/fonts/map/{pdftex,dvips}//",
             ENCFONTS        = ".;$TEXMF/fonts/data//;$TEXMF/fonts/enc/{dvips,pdftex}//",
@@ -175,7 +175,7 @@
             -- In an edit cycle it can be handy to launch an editor. The
             -- preferred one can be set here.
 
-         -- ["pdfview.method"]           = "okular", -- default (often acrobat) xpdf okular
+         -- ["pdfview.method"]           = "see", -- default (often acrobat) xpdf okular
 
         },
 
