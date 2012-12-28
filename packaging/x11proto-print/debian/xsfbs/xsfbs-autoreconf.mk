#!/usr/bin/make -f
# $Id$

# Automagical conversion of autoreconf results into quilt patches.

# Copyright 2006 Eugene Konev
#
# Licensed under the GNU General Public License, version 2.  See the file
# /usr/share/common-licenses/GPL or <http://www.gnu.org/copyleft/gpl.txt>.

# The idea behind this is storing changes produced by autoreconf as a 
# separate patch on quilt stack (on top of stack actually).
# The only usable target here is 'autoreconf`. Other targets are not
# supposed to be called directly. DO NOT invoke them, unless you know what
# you are doing.
# The autoreconf target will check if files with names in $(RECONF_CHECKFILES)
# were changed during patching (from upstream version or from previously
# autoreconfed version) and call actual autoreconfing if they were.
# The actual autoreconfing target (doautoreconf) WILL FAIL after 
# calling autoreconf and pushing changes into quilt stack by design. It
# should never be invoked by automatic build process.
# The proposed use is adding autoreconf into clean's prerequisites before
# xsfclean like:
# - clean: xsfclean
# + clean: autoreconf xsfclean
# This will ensure it is called when you build package with dpkg-buildpackage.

# This dir will be used for producing diff of autoreconfed tree
RECONF_DIR := xsfautoreconf

# This files will be checked for changes
RECONF_CHECKFILES += configure.ac Makefile.am

# This files will not be hardlinked but copied
RECONF_NOLINKFILES += aclocal.m4

# This files/dirs will be pruned after autoreconf run
RECONF_PRUNEFILES += autom4te.cache config.h.in~ aclocal.m4~

# Internal target. Never invoke directly.
stampdir_target+=check.md5sum
$(STAMP_DIR)/check.md5sum:
	dh_testdir
	$(MAKE) -f debian/rules prepare
	for F in $(RECONF_CHECKFILES); do \
	  find . -wholename ./$(STAMP_DIR) -prune -o -name $$F -print | \
	    LC_ALL=C sort | xargs --no-run-if-empty md5sum >>$@; \
	done

# Internal target. Never invoke directly.
$(STAMP_DIR)/clean.md5sum:
	dh_testdir
	$(MAKE) -f debian/rules unpatch
	rm -f $(STAMP_DIR)/check.md5sum
	$(MAKE) -f debian/rules $(STAMP_DIR)/check.md5sum
	mv $(STAMP_DIR)/check.md5sum $@

# Internal target. Never invoke directly.
debian/patches/patched.md5sum:
	dh_testdir
	[ -f $(STAMP_DIR)/clean.md5sum ] || \
	  $(MAKE) -f debian/rules $(STAMP_DIR)/clean.md5sum

	$(MAKE) -f debian/rules patch
	rm -f $(STAMP_DIR)/check.md5sum
	$(MAKE) -f debian/rules $(STAMP_DIR)/check.md5sum
	if ! diff $(STAMP_DIR)/clean.md5sum \
	          $(STAMP_DIR)/check.md5sum > /dev/null; then \
	  $(MAKE) -f debian/rules doautoreconf; \
	else \
	  mv $(STAMP_DIR)/check.md5sum $@; \
	fi

# Internal target. Never invoke directly.
,PHONY: doautoreconf
doautoreconf: patch
	quilt push -a >>$(STAMP_DIR)/log/autoreconf 2>&1 || true
	if quilt applied | grep ^autoreconf.diff$$ > /dev/null; then \
	  quilt pop -a >>$(STAMP_DIR)/log/autoreconf 2>&1; \
	  quilt rename -p autoreconf.diff autoreconf-old.diff \
	       >>$(STAMP_DIR)/log/autoreconf 2>&1; \
	  quilt delete autoreconf-old.diff >>$(STAMP_DIR)/log/autoreconf 2>&1; \
	  quilt push -a >>$(STAMP_DIR)/log/autoreconf 2>&1; \
	fi

	if [ -e $(RECONF_DIR) ]; then \
	  echo "ERROR: $(RECONF_DIR) already exists. Cleanup by hand"; \
	  exit 1; \
	fi

	mkdir -p $(RECONF_DIR)/before
	find . -maxdepth 1 -mindepth 1 ! -wholename ./$(RECONF_DIR) \
	     -a ! -wholename ./debian -a ! -wholename ./patches \
	     -a ! -wholename ./.pc -a ! -wholename ./$(STAMP_DIR) | \
	  xargs -i{} cp -al {} $(RECONF_DIR)/before/

	for F in $(RECONF_PRUNEFILES); do \
	  find $(RECONF_DIR)/before -name $$F -print | \
	    xargs --no-run-if-empty rm -r; \
	done

	cp -al $(RECONF_DIR)/before $(RECONF_DIR)/after

	for F in $(RECONF_NOLINKFILES); do \
	  find . -wholename ./$(RECONF_DIR) -prune -o -wholename ./debian \
	       -prune -o -wholename ./$(STAMP_DIR) -prune -o -name $$F \
	       -print | \
	    xargs --no-run-if-empty -i{} cp --remove-destination {} \
	      $(RECONF_DIR)/after/{}; \
	done

	cd $(RECONF_DIR)/after && autoreconf -v --install && \
	  for F in $(RECONF_PRUNEFILES); do \
	    find . -name $$F -print | \
	      xargs --no-run-if-empty rm -r; \
	  done

	cd $(RECONF_DIR) && diff -Nru before after > autoreconf.diff || true

	quilt import $(RECONF_DIR)/autoreconf.diff \
	      >>$(STAMP_DIR)/log/autoreconf 2>&1

	mv $(STAMP_DIR)/check.md5sum debian/patches/patched.md5sum

	rm -r $(RECONF_DIR) && rm -f patches/autoreconf-old.diff

	@echo 
	@echo "****************************************************************"
	@echo "  This target is made to fail INTENTIONALLY. It should NEVER    "
	@echo "  be invoked during automatic builds.                           "
	@echo 
	@echo "  This target was invoked because you added/removed/changed     "
	@echo "  patches which modify either configure.ac or Makefile.am and,  "
	@echo "  thus, require autoreconf run. And all autoreconfing should    "
	@echo "  happen before uploading.                                      "
	@echo 
	@echo "  (See also debian/xsfbs/xsfbs-autoreconf.mk)                   "
	@echo 
	@echo "  If you see this message, autoreconfing actually SUCCEEDED,    "
	@echo "  and your build should finish successfully, when rerun.        "
	@echo "****************************************************************"
	@echo 
	exit 1;

.PHONY: autoreconf
autoreconf: debian/patches/patched.md5sum patch $(STAMP_DIR)/check.md5sum
	if ! diff $(STAMP_DIR)/check.md5sum \
	          debian/patches/patched.md5sum > /dev/null; then \
	  $(MAKE) -f debian/rules doautoreconf; \
	fi
