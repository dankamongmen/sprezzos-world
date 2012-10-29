############################ -*- Mode: Makefile -*- ###########################
## local.mk --- 
## Author           : Manoj Srivastava ( srivasta@glaurung.green-gryphon.com ) 
## Created On       : Sat Nov 15 10:42:10 2003
## Created On Node  : glaurung.green-gryphon.com
## Last Modified By : Manoj Srivastava
## Last Modified On : Thu Apr  3 02:33:14 2008
## Last Machine Used: anzu.internal.golden-gryphon.com
## Update Count     : 21
## Status           : Unknown, Use with caution!
## HISTORY          : 
## Description      : 
## 
## arch-tag: b07b1015-30ba-4b46-915f-78c776a808f4
## 
###############################################################################

testdir:
	$(testdir)

debian/stamp/pre-config-common: debian/stamp/conf/make
debian/stamp/BUILD/make:        debian/stamp/build/make
debian/stamp/INST/make:         debian/stamp/install/make
debian/stamp/BIN/make:          debian/stamp/binary/make

CLN-common::
	$(REASON)
	-test ! -f Makefile || $(MAKE) distclean

CLEAN/make::
	-rm -rf $(TMPTOP)

debian/stamp/conf/make:
	$(checkdir)
	$(REASON)
	@test -d debian/stamp/conf || mkdir -p debian/stamp/conf
	WARNINGS=none autoreconf --warnings=none -f -i 
	ac_cv_lib_util_getloadavg=no ./configure                   \
               --verbose --prefix=$(PREFIX) --mandir=$(MANDIR)         \
               --infodir=$(INFODIR) --sysconfdir=/etc                  \
                  $(confflags)
	@echo done > $@

debian/stamp/build/make:
	$(checkdir)
	$(REASON)
	@test -d debian/stamp/build || mkdir -p debian/stamp/build
	$(MAKE) CC="$(CC)" CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)"
	$(check-libraries)
	@echo done > $@



debian/stamp/install/make:
	$(checkdir)
	$(REASON)
	$(TESTROOT)
	rm -rf               $(TMPTOP)
	$(make_directory)    $(TMPTOP)
	$(make_directory)    $(BINDIR)
	$(make_directory)    $(MAN1DIR)
	$(make_directory)    $(DOCDIR)
	$(make_directory)    $(LINTIANDIR)
	echo '$(package): description-synopsis-might-not-be-phrased-properly'>> \
                              $(LINTIANDIR)/$(package)
	$(MAKE)              $(INT_INSTALL_TARGET)  prefix=$(TMPTOP)/usr \
	                     infodir=$(INFODIR)     mandir=$(MANDIR)     \
                              INSTALL_PROGRAM="$(install_program)"
	$(install_file)      README                 $(DOCDIR)/README
	$(install_file)      debian/Explanations    $(DOCDIR)/
	$(install_file)      debian/changelog       $(DOCDIR)/changelog.Debian
	$(install_file)      ChangeLog              $(DOCDIR)/changelog
	$(install_file)      NEWS                   $(DOCDIR)/NEWS
	$(install_file)      debian/NEWS.Debian     $(DOCDIR)/
	gzip -9frq           $(DOCDIR)/
# Make sure the copyright file is not compressed
	$(install_file)      debian/copyright       $(DOCDIR)/copyright
	gzip -9fqr           $(MANDIR)/
	rmdir --ignore-fail-on-non-empty            $(LIBDIR) || true
	@test ! -g           $(BINDIR)/make || \
              echo "WARNING! WARNING! removing setgid bits from make\n\n\g"
	test ! -g            $(BINDIR)/make     || chgrp root $(BINDIR)/make
	test ! -g            $(BINDIR)/make     || chmod 0755 $(BINDIR)/make
	test ! -e            $(INFODIR)/dir     || rm -f $(INFODIR)/dir
	test ! -e            $(INFODIR)/dir.old || rm -f $(INFODIR)/dir.old
	$(strip-exec)
	@test -d debian/stamp/install || mkdir -p debian/stamp/install
	@echo done > $@

debian/stamp/binary/make:
	$(checkdir)
	$(REASON)
	$(TESTROOT)
	$(make_directory)    $(TMPTOP)/DEBIAN
	$(get-shlib-deps)
	dpkg-gencontrol      -p$(package) -isp      -P$(TMPTOP)
	$(create_md5sum)     $(TMPTOP)
	chown -R root:root   $(TMPTOP)
	chmod -R u+w,go=rX   $(TMPTOP)
	dpkg --build         $(TMPTOP) ..
	@test -d debian/stamp/binary || mkdir -p debian/stamp/binary
	@echo done > $@
