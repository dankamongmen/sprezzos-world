Adjust the texmfcnf.lua file to realities in Debian
---
 texmf-dist/web2c/texmfcnf.lua |   30 ++++++++++++++----------------
 1 file changed, 14 insertions(+), 16 deletions(-)

--- texlive-base-2013.20130509.orig/texmf-dist/web2c/texmfcnf.lua
+++ texlive-base-2013.20130509/texmf-dist/web2c/texmfcnf.lua
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
-            TEXMFVAR        = "home:.texlive2013/texmf-var",
+            TEXMFSYSVAR     = "/var/lib/texmf",
+            TEXMFVAR        = "home:.texmf-var",
 
             -- We have only one cache path but there can be more. The first writable one
             -- will be chosen but there can be more readable paths.
 
             TEXMFCACHE      = "$TEXMFSYSVAR;$TEXMFVAR",
-            TEXMFCONFIG     = "home:.texlive2013/texmf-config",
+            TEXMFCONFIG     = "home:.texmf-config",
 
             -- I don't like this texmf under home and texmf-home would make more
             -- sense. One never knows what installers put under texmf anywhere and
@@ -61,13 +59,13 @@
             -- By using prefixes we don't get expanded paths in the cache __path__
             -- entry. This makes the tex root relocatable.
 
-            TEXMFOS         = "selfautodir:",
-            TEXMFDIST       = "selfautoparent:texmf-dist",
-
-            TEXMFLOCAL      = texmflocal,
-            TEXMFSYSCONFIG  = "selfautoparent:texmf-config",
-            TEXMFFONTS      = "selfautoparent:texmf-fonts",
-            TEXMFPROJECT    = "selfautoparent:texmf-project",
+            -- TEXMFOS         = "selfautodir:",
+            TEXMFDIST       = "/usr/share/texlive/texmf-dist",
+            TEXMFDEBIAN     = "/usr/share/texmf",
+            TEXMFLOCAL      = "/usr/local/share/texmf",
+            TEXMFSYSCONFIG  = "/etc/texmf",
+            -- TEXMFFONTS      = "selfautoparent:texmf-fonts",
+            -- TEXMFPROJECT    = "selfautoparent:texmf-project",
 
             TEXMFHOME       = "home:texmf",
          -- TEXMFHOME       = os.name == "macosx" and "home:Library/texmf" or "home:texmf",
@@ -75,7 +73,7 @@
             -- We need texmfos for a few rare files but as I have a few more bin trees
             -- a hack is needed. Maybe other users also have texmf-platform-new trees.
 
-            TEXMF           = "{$TEXMFCONFIG,$TEXMFHOME,!!$TEXMFSYSCONFIG,!!$TEXMFSYSVAR,!!$TEXMFPROJECT,!!$TEXMFFONTS,!!$TEXMFLOCAL,!!$TEXMFDIST}",
+            TEXMF           = "{$TEXMFCONFIG,$TEXMFHOME,!!$TEXMFSYSCONFIG,!!$TEXMFSYSVAR,!!$TEXMFLOCAL,!!$TEXMFDEBIAN,!!$TEXMFDIST}",
 
             TEXFONTMAPS     = ".;$TEXMF/fonts/data//;$TEXMF/fonts/map/{pdftex,dvips}//",
             ENCFONTS        = ".;$TEXMF/fonts/data//;$TEXMF/fonts/enc/{dvips,pdftex}//",
@@ -174,7 +172,7 @@
             -- In an edit cycle it can be handy to launch an editor. The
             -- preferred one can be set here.
 
-         -- ["pdfview.method"]           = "okular", -- default (often acrobat) xpdf okular
+         -- ["pdfview.method"]           = "see", -- default (often acrobat) xpdf okular
 
         },
 
