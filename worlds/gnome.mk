.PHONY: gconf
gconf:$(GCONF)_$(ARCH).deb
$(GCONF): $(SPREZZ)/gconf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf GConf-$(gconf_UPVER).tar.xz $(TARARGS) $@

.PHONY: gconf-editor
gconf-editor:$(GCONFEDITOR)_$(ARCH).deb
$(GCONFEDITOR): $(SPREZZ)/gconf-editor/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gconf-editor-$(gconf-editor_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-documents
gnome-documents:$(GNOMEDOCUMENTS)_$(ARCH).deb
$(GNOMEDOCUMENTS): $(SPREZZ)/gnome-documents/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-documents-$(gnome-documents_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-icon-theme
gnome-icon-theme:$(GNOMEICONTHEME)_$(ARCH).deb
$(GNOMEICONTHEME): $(SPREZZ)/gnome-icon-theme/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-icon-theme-$(gnome-icon-theme_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-mime-data
gnome-mime-data:$(GNOMEMIMEDATA)_$(ARCH).deb
$(GNOMEMIMEDATA): $(SPREZZ)/gnome-mime-data/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnome-mime-data-$(gnome-mime-data_UPVER).tar.gz $(TARARGS) $@

.PHONY: libgdata
libgdata:$(LIBGDATA)_$(ARCH).deb
$(LIBGDATA): $(SPREZZ)/libgdata/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgdata-$(libgdata_UPVER).tar.xz $(TARARGS) $@

.PHONY: libgsf
libgsf:$(LIBGSF)_$(ARCH).deb
$(LIBGSF): $(SPREZZ)/libgsf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgsf-$(libgsf_UPVER).tar.xz $(TARARGS) $@

.PHONY: libsocialweb
libsocialweb:$(LIBSOCIALWEB)_$(ARCH).deb
$(LIBSOCIALWEB): $(SPREZZ)/libsocialweb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libsocialweb-$(libsocialweb_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgnome-media-profiles
libgnome-media-profiles:$(LIBGNOMEMEDIAPROFILES)_$(ARCH).deb
$(LIBGNOMEMEDIAPROFILES): $(SPREZZ)/libgnome-media-profiles/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libgnome-media-profiles-$(libgnome-media-profiles_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: tango-icon-theme
tango-icon-theme:$(TANGOICONTHEME)_$(ARCH).deb
$(TANGOICONTHEME): $(SPREZZ)/tango-icon-theme/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf tango-icon-theme-$(tango-icon-theme_UPVER).tar.gz $(TARARGS) $@
	
.PHONY: vte3
vte3:$(VTE3)_$(ARCH).deb
$(VTE3): $(SPREZZ)/vte3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf vte_$(vte3_UPVER).orig.tar.xz $(TARARGS) $@

