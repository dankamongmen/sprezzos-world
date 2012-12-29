.PHONY: gnome-icon-theme
gnome-icon-theme:$(GNOMEICONTHEME)_$(ARCH).deb
$(GNOMEICONTHEME): $(SPREZZ)/gnome-icon-theme/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-icon-theme_$(gnome-icon-theme_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: tango-icon-theme
tango-icon-theme:$(TANGOICONTHEME)_$(ARCH).deb
$(TANGOICONTHEME): $(SPREZZ)/tango-icon-theme/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tango-icon-theme-$(tango-icon-theme_UPVER).tar.gz $(TARARGS) $@
