.PHONY: mhash
mhash:$(MHASH)_$(ARCH).deb
$(MHASH): $(SPREZZ)/mhash/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mhash-$(mhash_UPVER).tar.gz $(TARARGS) $@

.PHONY: tcl8.5
tcl8.5:$(TCL8.5)_$(ARCH).deb
$(TCL8.5): $(SPREZZ)/tcl8.5/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tcl$(tcl8.5_UPVER)-src.tar.gz $(TARARGS) $@

.PHONY: shared-mime-info
shared-mime-info:$(SHAREDMIMEINFO)_$(ARCH).deb
$(SHAREDMIMEINFO): $(SPREZZ)/shared-mime-info/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf shared-mime-info-$(shared-mime-info_UPVER).tar.xz $(TARARGS) $@

.PHONY: libassuan
libassuan:$(LIBASSUAN)_$(ARCH).deb
$(LIBASSUAN): $(SPREZZ)/libassuan/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libassuan-$(libassuan_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libyaml
libyaml:$(LIBYAML)_$(ARCH).deb
$(LIBYAML): $(SPREZZ)/libyaml/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf yaml-$(libyaml_UPVER).tar.gz $(TARARGS) $@

.PHONY: lua5.1
lua5.1:$(LUA5.1)_$(ARCH).deb
$(LUA5.1): $(SPREZZ)/lua5.1/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf lua-$(lua5.1_UPVER).tar.gz $(TARARGS) $@

.PHONY: libthai
libthai:$(LIBTHAI)_$(ARCH).deb
$(LIBTHAI): $(SPREZZ)/libthai/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libthai-$(libthai_UPVER).tar.gz $(TARARGS) $@

.PHONY: openntpd
openntpd:$(OPENNTPD)_$(ARCH).deb
$(OPENNTPD): $(SPREZZ)/openntpd/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf openntpd-$(openntpd_UPVER).tar.gz $(TARARGS) $@

.PHONY: celt
celt:$(CELT)_$(ARCH).deb
$(CELT): $(SPREZZ)/celt/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf celt-$(celt_UPVER).tar.gz $(TARARGS) $@

.PHONY: gzip
gzip:$(GZIP)_$(ARCH).deb
$(GZIP): $(SPREZZ)/gzip/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gzip-$(gzip_UPVER).tar.gz $(TARARGS) $@

.PHONY: ispell
ispell:$(ISPELL)_$(ARCH).deb
$(ISPELL): $(SPREZZ)/ispell/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ispell-$(ispell_UPVER).tar.gz $(TARARGS) $@

.PHONY: discover
discover:$(DISCOVER)_$(ARCH).deb
$(DISCOVER): $(SPREZZ)/discover/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf discover-$(discover_UPVER).tar.gz $(TARARGS) $@

.PHONY: qca2
qca2:$(QCA2)_$(ARCH).deb
$(QCA2): $(SPREZZ)/qca2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf qca-$(qca2_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libwlocate
libwlocate:$(LIBWLOCATE)_$(ARCH).deb
$(LIBWLOCATE): $(SPREZZ)/libwlocate/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libwlocate-$(libwlocate_UPVER).tar.gz $(TARARGS) $@

.PHONY: shapelib
shapelib:$(SHAPELIB)_$(ARCH).deb
$(SHAPELIB): $(SPREZZ)/shapelib/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shapelib-$(shapelib_UPVER).tar.gz $(TARARGS) $@

.PHONY: shelltestrunner
shelltestrunner:$(SHELLTESTRUNNER)_$(ARCH).deb
$(SHELLTESTRUNNER): $(SPREZZ)/shelltestrunner/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf shelltestrunner-$(shelltestrunner_UPVER).tar.gz $(TARARGS) $@

.PHONY: openal-soft
openal-soft:$(OPENALSOFT)_$(ARCH).deb
$(OPENALSOFT): $(SPREZZ)/openal-soft/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf openal-soft-$(openal-soft_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: bibutils
bibutils:$(BIBUTILS)_$(ARCH).deb
$(BIBUTILS): $(SPREZZ)/bibutils/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bibutils_$(bibutils_UPVER)_src.tgz $(TARARGS) $@

.PHONY: tcltk-defaults
tcltk-defaults:$(TCLTKDEFAULTS)_$(ARCH).deb
$(TCLTKDEFAULTS): $(SPREZZ)/tcltk-defaults/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tcltk-defaults-$(tcltk-defaults_UPVER).tar.gz $(TARARGS) $@

.PHONY: tk8.5
tk8.5:$(TK8.5)_$(ARCH).deb
$(TK8.5): $(SPREZZ)/tk8.5/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tk$(tk8.5_UPVER)-src.tar.gz $(TARARGS) $@

.PHONY: prison
prison:$(PRISON)_$(ARCH).deb
$(PRISON): $(SPREZZ)/prison/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf prison-$(prison_UPVER).tar.gz $(TARARGS) $@

.PHONY: bzr
bzr:$(BZR)_$(ARCH).deb
$(BZR): $(SPREZZ)/bzr/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf bzr-$(bzr_UPVER).tar.gz $(TARARGS) $@

.PHONY: mercurial
mercurial:$(MERCURIAL)_$(ARCH).deb
$(MERCURIAL): $(SPREZZ)/mercurial/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf mercurial-$(mercurial_UPVER).tar.gz $(TARARGS) $@

.PHONY: metapixel
metapixel:$(METAPIXEL)_$(ARCH).deb
$(METAPIXEL): $(SPREZZ)/metapixel/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf metapixel-$(metapixel_UPVER).tar.gz $(TARARGS) $@

.PHONY: nfswatch
nfswatch:$(NFSWATCH)_$(ARCH).deb
$(NFSWATCH): $(SPREZZ)/nfswatch/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf nfswatch-$(nfswatch_UPVER).tar.gz $(TARARGS) $@

.PHONY: palp
palp:$(PALP)_$(ARCH).deb
$(PALP): $(SPREZZ)/palp/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf palp-$(palp_UPVER).tar.gz $(TARARGS) $@

.PHONY: libldm
libldm:$(LIBLDM)_$(ARCH).deb
$(LIBLDM): $(SPREZZ)/libldm/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libldm-$(libldm_UPVER).tar.gz $(TARARGS) $@

.PHONY: rfkill
rfkill:$(RFKILL)_$(ARCH).deb
$(RFKILL): $(SPREZZ)/rfkill/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf rfkill-$(rfkill_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: ppp
ppp:$(PPP)_$(ARCH).deb
$(PPP): $(SPREZZ)/ppp/debian/changelog
	cp -r $(SPREZZ)/ppp $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ppp-$(ppp_UPVER).tar.gz $(TARARGS) $@

.PHONY: apg
apg:$(APG)_$(ARCH).deb
$(APG): $(SPREZZ)/apg/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf apg-$(apg_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgusb
libgusb:$(LIBGUSB)_$(ARCH).deb
$(LIBGUSB): $(SPREZZ)/libgusb/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgusb-$(libgusb_UPVER).tar.xz $(TARARGS) $@

.PHONY: folks
folks:$(FOLKS)_$(ARCH).deb
$(FOLKS): $(SPREZZ)/folks/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf folks-$(folks_UPVER).tar.xz $(TARARGS) $@

.PHONY: libavc1394
libavc1394:$(LIBAVC1394)_$(ARCH).deb
$(LIBAVC1394): $(SPREZZ)/libavc1394/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf libavc1394-$(libavc1394_UPVER).tar.gz $(TARARGS) $@

.PHONY: i2c-tools
i2c-tools:$(I2CTOOLS)_$(ARCH).deb
$(I2CTOOLS): $(SPREZZ)/i2c-tools/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf i2c-tools-$(i2c-tools_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: djvulibre
djvulibre:$(DJVULIBRE)_$(ARCH).deb
$(DJVULIBRE): $(SPREZZ)/djvulibre/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf djvulibre-$(djvulibre_UPVER).tar.gz $(TARARGS) $@
	rm -rf $@/debian
	cp -r $(<D) $@

