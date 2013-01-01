.PHONY: filelight
filelight:$(FILELIGHT)_$(ARCH).deb
$(FILELIGHT): $(SPREZZ)/filelight/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf filelight_$(filelight_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: hupnp
hupnp:$(HUPNP)_$(ARCH).deb
$(HUPNP): $(SPREZZ)/hupnp/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf hupnp_$(hupnp_UPVER).orig.tar.gz $(TARARGS) $@

.PHONY: kaccessible
kaccessible:$(KACCESSIBLE)_$(ARCH).deb
$(KACCESSIBLE): $(SPREZZ)/kaccessible/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kaccessible-$(kaccessible_UPVER).tar.xz $(TARARGS) $@

.PHONY: kactivities
kactivities:$(KACTIVITIES)_$(ARCH).deb
$(KACTIVITIES): $(SPREZZ)/kactivities/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kactivities_$(kactivities_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kde4libs
kde4libs:$(KDE4LIBS)_$(ARCH).deb
$(KDE4LIBS): $(SPREZZ)/kde4libs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kde4libs_$(kde4libs_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kdeartwork
kdeartwork:$(KDEARTWORK)_$(ARCH).deb
$(KDEARTWORK): $(SPREZZ)/kdeartwork/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kdeartwork_$(kdeartwork_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kde-baseapps
kde-baseapps:$(KDEBASEAPPS)_$(ARCH).deb
$(KDEBASEAPPS): $(SPREZZ)/kde-baseapps/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kde-baseapps_$(kde-baseapps_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kdepimlibs
kdepimlibs:$(KDEPIMLIBS)_$(ARCH).deb
$(KDEPIMLIBS): $(SPREZZ)/kdepimlibs/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kdepimlibs_$(kdepimlibs_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: kde-workspace
kde-workspace:$(KDEWORKSPACE)_$(ARCH).deb
$(KDEWORKSPACE): $(SPREZZ)/kde-workspace/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf kde-workspace_$(kde-workspace_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: oxygen-icons
oxygen-icons:$(OXYGENICONS)_$(ARCH).deb
$(OXYGENICONS): $(SPREZZ)/oxygen-icons/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf oxygen-icons-$(oxygen-icons_UPVER).tar.xz $(TARARGS) $@

.PHONY: qt4-x11
qt4-x11:$(QT4X11)_$(ARCH).deb
$(QT4X11): $(SPREZZ)/qt4-x11/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf qt-everywhere-opensource-src-$(qt4-x11_UPVER).tar.gz $(TARARGS) $@

.PHONY: qwt
qwt:$(QWT)_$(ARCH).deb
$(QWT): $(SPREZZ)/qwt/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf qwt-$(qwt_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: qscintilla2
qscintilla2:$(QSCINTILLA2)_$(ARCH).deb
$(QSCINTILLA2): $(SPREZZ)/qscintilla2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf QScintilla-gpl-$(qscintilla2_UPVER).tar.gz $(TARARGS) $@

.PHONY: simon
simon:$(SIMON)_$(ARCH).deb
$(SIMON): $(SPREZZ)/simon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf simon-$(simon_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: soprano
soprano:$(SOPRANO)_$(ARCH).deb
$(SOPRANO): $(SPREZZ)/soprano/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf soprano-$(soprano_UPVER).tar.bz2 $(TARARGS) $@

