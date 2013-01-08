.PHONY: evolution
evolution:$(EVOLUTION)_$(ARCH).deb
$(EVOLUTION): $(SPREZZ)/evolution/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf evolution-$(evolution_UPVER).tar.xz $(TARARGS) $@

.PHONY: evolution-data-server
evolution-data-server:$(EVOLUTIONDATASERVER)_$(ARCH).deb
$(EVOLUTIONDATASERVER): $(SPREZZ)/evolution-data-server/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf evolution-data-server-$(evolution-data-server_UPVER).tar.xz $(TARARGS) $@

.PHONY: evolution-mapi
evolution-mapi:$(EVOLUTIONMAPI)_$(ARCH).deb
$(EVOLUTIONMAPI): $(SPREZZ)/evolution-mapi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf evolution-mapi-$(evolution-mapi_UPVER).tar.xz $(TARARGS) $@

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

.PHONY: ghemical
ghemical:$(GHEMICAL)_$(ARCH).deb
$(GHEMICAL): $(SPREZZ)/ghemical/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghemical-$(ghemical_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-documents
gnome-documents:$(GNOMEDOCUMENTS)_$(ARCH).deb
$(GNOMEDOCUMENTS): $(SPREZZ)/gnome-documents/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-documents-$(gnome-documents_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-dvb-daemon
gnome-dvb-daemon:$(GNOMEDVBDAEMON)_$(ARCH).deb
$(GNOMEDVBDAEMON): $(SPREZZ)/gnome-dvb-daemon/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-dvb-daemon-$(gnome-dvb-daemon_UPVER).tar.xz $(TARARGS) $@

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

.PHONY: gst-plugins-bad1.0
gst-plugins-bad1.0:$(GSTPLUGINSBAD1.0)_$(ARCH).deb
$(GSTPLUGINSBAD1.0): $(SPREZZ)/gst-plugins-bad1.0/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gst-plugins-bad-$(gst-plugins-bad1.0_UPVER).tar.xz $(TARARGS) $@

.PHONY: gst-plugins-base
gst-plugins-base:$(GSTPLUGINSBASE)_$(ARCH).deb
$(GSTPLUGINSBASE): $(SPREZZ)/gst-plugins-base/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gst-plugins-base-$(gst-plugins-base_UPVER).tar.xz $(TARARGS) $@

.PHONY: gstreamer
gstreamer:$(GSTREAMER)_$(ARCH).deb
$(GSTREAMER): $(SPREZZ)/gstreamer/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gstreamer-$(gstreamer_UPVER).tar.xz $(TARARGS) $@

.PHONY: libgdata
libgdata:$(LIBGDATA)_$(ARCH).deb
$(LIBGDATA): $(SPREZZ)/libgdata/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgdata-$(libgdata_UPVER).tar.xz $(TARARGS) $@

.PHONY: libgnome-media-profiles
libgnome-media-profiles:$(LIBGNOMEMEDIAPROFILES)_$(ARCH).deb
$(LIBGNOMEMEDIAPROFILES): $(SPREZZ)/libgnome-media-profiles/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libgnome-media-profiles-$(libgnome-media-profiles_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgsf
libgsf:$(LIBGSF)_$(ARCH).deb
$(LIBGSF): $(SPREZZ)/libgsf/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgsf-$(libgsf_UPVER).tar.xz $(TARARGS) $@

.PHONY: libgtop2
libgtop2:$(LIBGTOP2)_$(ARCH).deb
$(LIBGTOP2): $(SPREZZ)/libgtop2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgtop-$(libgtop2_UPVER).tar.xz $(TARARGS) $@

.PHONY: libpeas
libpeas:$(LIBPEAS)_$(ARCH).deb
$(LIBPEAS): $(SPREZZ)/libpeas/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version --repack
	tar xzvf libpeas-$(libpeas_UPVER).tar.gz $(TARARGS) $@

.PHONY: libsocialweb
libsocialweb:$(LIBSOCIALWEB)_$(ARCH).deb
$(LIBSOCIALWEB): $(SPREZZ)/libsocialweb/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libsocialweb-$(libsocialweb_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libsigc++-2.0
libsigc++-2.0:$(LIBSIGC++2.0)_$(ARCH).deb
$(LIBSIGC++2.0): $(SPREZZ)/libsigc++-2.0/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libsigc++-$(libsigc++-2.0_UPVER).tar.xz $(TARARGS) $@

.PHONY: libwnck
libwnck:$(LIBWNCK)_$(ARCH).deb
$(LIBWNCK): $(SPREZZ)/libwnck/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libwnck-$(libwnck_UPVER).tar.xz $(TARARGS) $@

.PHONY: libwnck3
libwnck3:$(LIBWNCK3)_$(ARCH).deb
$(LIBWNCK3): $(SPREZZ)/libwnck3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libwnck3-$(libwnck3_UPVER).tar.xz $(TARARGS) $@

.PHONY: seed
seed:$(SEED)_$(ARCH).deb
$(SEED): $(SPREZZ)/seed/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf seed-$(seed_UPVER).tar.xz $(TARARGS) $@

.PHONY: syncevolution
syncevolution:$(SYNCEVOLUTION)_$(ARCH).deb
$(SYNCEVOLUTION): $(SPREZZ)/syncevolution/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf syncevolution_$(syncevolution_UPVER).orig.tar.gz $(TARARGS) $@

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
