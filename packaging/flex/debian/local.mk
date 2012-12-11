############################ -*- Mode: Makefile -*- ###########################
## local.mk --- 
## Author           : Manoj Srivastava ( srivasta@glaurung.green-gryphon.com ) 
## Created On       : Sat Nov 15 10:42:10 2003
## Created On Node  : glaurung.green-gryphon.com
## Last Modified By : Manoj Srivastava
## Last Modified On : Fri Oct 20 14:14:16 2006
## Last Machine Used: glaurung.internal.golden-gryphon.com
## Update Count     : 48
## Status           : Unknown, Use with caution!
## HISTORY          : 
## Description      : 
## 
## arch-tag: b07b1015-30ba-4b46-915f-78c776a808f4
## 
###############################################################################

testdir:
	$(testdir)

debian/stamp/pre-config-common: debian/stamp/conf/flex
debian/stamp/BUILD/flex:        debian/stamp/build/flex
debian/stamp/INST/flex:         debian/stamp/install/flex
debian/stamp/BIN/flex:          debian/stamp/binary/flex

debian/stamp/BUILD/flex-doc:    debian/stamp/build/flex-doc
debian/stamp/INST/flex-doc:     debian/stamp/install/flex-doc
debian/stamp/BIN/flex-doc:      debian/stamp/binary/flex-doc

CLN-common::
	$(REASON)
	$(MAKE) clean -C po || true
	rm -f doc/flex.info doc/flex.info-[0-9] doc/flex.info-[0-9][0-9]

CLEAN/flex::
	-rm -rf $(TMPTOP)

CLEAN/flex-doc::
	-rm -rf $(TMPTOP)

debian/stamp/conf/flex:
	$(checkdir)
	$(REASON)
	@test -d debian/stamp/conf || mkdir -p debian/stamp/conf
	if ! which flex >/dev/null 2>&1; then                              \
           if [ -e debian/scan.l.md5sum ] &&                               \
              [ "`md5sum scan.l`" = "`cat debian/scan.l.md5sum`" ]; then   \
                if [ scan.l -nt scan.c ]; then                             \
                   echo "scan.l newer than scan.c";                        \
                fi;                                                        \
                touch scan.c;                                              \
           fi;                                                             \
        fi
	WARNINGS=none autoreconf --warnings=none -f -i 
	ac_cv_lib_util_getloadavg=no ./configure   --verbose           \
                --prefix=$(PREFIX) $(confflags)
	@echo done > $@

debian/stamp/build/flex:
	$(checkdir)
	$(REASON)
	$(CHECKPO)
	@test -d debian/stamp/build || mkdir -p debian/stamp/build
	bash -n debian/doc.postinst
	bash -n debian/doc.prerm
	bash -n debian/postinst
	bash -n debian/prerm
	$(MAKE) CC="$(CC)" CFLAGS="$(CFLAGS)" LDFLAGS="$(LDFLAGS)"
	$(MAKE) -C po
ifneq (,$(filter nocheck,$(DEB_BUILD_OPTIONS)))
  ifeq ($(DEB_BUILD_GNU_TYPE),$(DEB_HOST_GNU_TYPE))
	$(MAKE) check
  endif
endif
	$(check-libraries)
	@echo done > $@

debian/stamp/build/flex-doc:
	$(checkdir)
	$(REASON)
	@test -d debian/stamp/build || mkdir -p debian/stamp/build
	$(MAKE) -C doc flex.info flex.html
	@echo done > $@

debian/stamp/install/flex-doc:
	$(checkdir)
	$(REASON)
	$(TESTROOT)
	rm -rf              $(TMPTOP)
	$(make_directory)   $(TMPTOP)
	$(make_directory)   $(DOCDIR)/html
	$(make_directory)   $(DOCBASEDIR)
	$(make_directory)   $(LINTIANDIR)
	$(install_file)     debian/doc.lintian.overrides $(LINTIANDIR)/$(package)
	$(install_file)     README                  $(DOCDIR)/README
	$(install_file)     NEWS                    $(DOCDIR)/NEWS
	$(install_file)     debian/changelog        $(DOCDIR)/changelog.Debian
	gzip -9fqr          $(DOCDIR)/
# Make sure the copyright file is not compressed
	$(install_file)     debian/copyright        $(DOCDIR)/copyright
	$(install_file)     doc/flex.html/*         $(DOCDIR)/html/
	ln -s               NEWS.gz                 $(DOCDIR)/changelog.gz 
	$(install_file)     debian/docentry         $(DOCBASEDIR)/$(package)
	@test -d debian/stamp/install || mkdir -p debian/stamp/install
	@echo done > $@

debian/stamp/install/flex:
	$(checkdir)
	$(REASON)
	$(TESTROOT)
	rm -rf               $(TMPTOP)
	$(make_directory)    $(TMPTOP)
	$(make_directory)    $(BINDIR)
	$(make_directory)    $(LIBDIR)
	$(make_directory)    $(INFODIR)
	$(make_directory)    $(MAN1DIR)
	$(make_directory)    $(DOCDIR)
	$(make_directory)    $(LINTIANDIR)
	$(install_file)      debian/lintian.overrides $(LINTIANDIR)/$(package)
	$(MAKE)              $(INT_INSTALL_TARGET)  prefix=$(TMPTOP)/usr \
	                     infodir=$(INFODIR)     mandir=$(MANDIR)     \
                              INSTALL_PROGRAM="$(install_program)"
	$(install_file)      debian/libfl.shared_object $(LIBDIR)/libfl.so
	$(MAKE) install      -C po                  infodir=$(INFODIR) prefix=$(TMPTOP)/usr
	$(MAKE) install-info -C doc                 infodir=$(INFODIR) prefix=$(TMPTOP)/usr
	ln -s                flex                   $(BINDIR)/flex++
	test ! -e            $(INFODIR)/dir         || rm -f $(INFODIR)/dir
	test ! -e            $(INFODIR)/dir.old     || rm -f $(INFODIR)/dir.old
	$(install_file)      README                 $(DOCDIR)/README
	$(install_file)      debian/README          $(DOCDIR)/README.Debian 
	$(install_file)      debian/NEWS.Debian     $(DOCDIR)/NEWS.Debian 
	$(install_file)      NEWS                   $(DOCDIR)/NEWS
	$(install_file)      debian/changelog       $(DOCDIR)/changelog.Debian
	gzip -9frq           $(DOCDIR)/
# Make sure the copyright file is not compressed
	$(install_file)      debian/copyright       $(DOCDIR)/copyright
	gzip -9fqr           $(MANDIR)/
	test ! -e            $(INFODIR)/dir        || rm -f $(INFODIR)/dir
	test ! -e            $(INFODIR)/dir.old    || rm -f $(INFODIR)/dir.old
	gzip -9fqr            $(INFODIR)
	if [ ! -e debian/scan.l.md5sum ] ||                                              \
           [ "`md5sum scan.l`" != "`cat debian/scan.l.md5sum`" ]; then                   \
              md5sum scan.l >      debian/scan.l.md5sum;                                 \
        fi
	$(strip-exec)
	$(strip-lib)
	ln -s 	             NEWS.gz                $(DOCDIR)/changelog.gz
	ln -s                flex.1.gz              $(MAN1DIR)/lex.1.gz
	ln -s                flex.1.gz              $(MAN1DIR)/flex++.1.gz
	ln -s                flex                   $(BINDIR)/lex
	ln -s                libfl.a                $(LIBDIR)/libl.a
	@test -d debian/stamp/install || mkdir -p debian/stamp/install
	@echo done > $@

debian/stamp/binary/flex-doc:
	$(checkdir)
	$(REASON)
	$(TESTROOT)
	$(make_directory)   $(TMPTOP)/DEBIAN
	$(install_script)    debian/doc.postinst     $(TMPTOP)/DEBIAN/postinst
	$(install_script)    debian/doc.prerm        $(TMPTOP)/DEBIAN/prerm
	$(install_script)    debian/doc.postrm       $(TMPTOP)/DEBIAN/postrm
	dpkg-gencontrol      -p$(package) -isp       -P$(TMPTOP)
	$(create_md5sum)     $(TMPTOP)
	chown -R root:root   $(TMPTOP)
	chmod -R u+w,go=rX   $(TMPTOP)
	dpkg --build         $(TMPTOP) ..
	@test -d debian/stamp/binary || mkdir -p debian/stamp/binary
	@echo done > $@

debian/stamp/binary/flex:
	$(checkdir)
	$(REASON)
	$(TESTROOT)
	$(make_directory)    $(TMPTOP)/DEBIAN
	$(install_script)    debian/postinst        $(TMPTOP)/DEBIAN/postinst
	$(install_script)    debian/prerm           $(TMPTOP)/DEBIAN/prerm
	$(get-shlib-deps)
	dpkg-gencontrol      -p$(package) -isp      -P$(TMPTOP)
	$(create_md5sum)     $(TMPTOP)
	chown -R root:root   $(TMPTOP)
	chmod -R u+w,go=rX   $(TMPTOP)
	dpkg --build         $(TMPTOP) ..
	@test -d debian/stamp/binary || mkdir -p debian/stamp/binary
	@echo done > $@
