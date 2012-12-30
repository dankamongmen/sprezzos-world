.PHONY: filelight
filelight:$(filelight)_$(ARCH).deb
$(filelight): $(SPREZZ)/filelight/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf filelight_$(filelight_UPVER).orig.tar.xz $(TARARGS) $@

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

.PHONY: kde-workspace
kde-workspace:$(kdeworkspace)_$(ARCH).deb
$(kdeworkspace): $(SPREZZ)/kde-workspace/debian/changelog
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
