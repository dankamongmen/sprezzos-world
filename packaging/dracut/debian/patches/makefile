Index: dracut/Makefile
===================================================================
--- dracut.orig/Makefile	2012-06-29 23:46:38.416153526 +0200
+++ dracut/Makefile	2012-06-29 23:47:18.000564847 +0200
@@ -13,10 +13,7 @@
 
 .PHONY: install clean archive rpm testimage test all check AUTHORS doc
 
-all: syncheck dracut-version.sh install/dracut-install
-
-install/dracut-install:
-	$(MAKE) -C install dracut-install
+all: syncheck dracut-version.sh
 
 doc: $(manpages) dracut.html
 
@@ -74,7 +71,6 @@
 		ln -s ../dracut-shutdown.service \
 		$(DESTDIR)$(systemdsystemunitdir)/shutdown.target.wants/dracut-shutdown.service; \
 	fi
-	$(MAKE) -C install install
 
 dracut-version.sh:
 	@echo "DRACUT_VERSION=$(VERSION)-$(GITVERSION)" > dracut-version.sh
@@ -87,7 +83,6 @@
 	$(RM) dracut-*.rpm dracut-*.tar.bz2
 	$(RM) $(manpages) dracut.html
 	$(MAKE) -C test clean
-	$(MAKE) -C install clean
 
 archive: dracut-$(VERSION)-$(GITVERSION).tar.bz2
 
