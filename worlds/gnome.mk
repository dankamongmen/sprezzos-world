.PHONY: anjuta
anjuta:$(ANJUTA)_$(ARCH).deb
$(ANJUTA): $(SPREZZ)/anjuta/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf anjuta-$(anjuta_UPVER).tar.xz $(TARARGS) $@

.PHONY: anjuta-extras
anjuta-extras:$(ANJUTAEXTRAS)_$(ARCH).deb
$(ANJUTAEXTRAS): $(SPREZZ)/anjuta-extras/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf anjuta-extras-$(anjuta-extras_UPVER).tar.xz $(TARARGS) $@

.PHONY: caribou
caribou:$(CARIBOU)_$(ARCH).deb
$(CARIBOU): $(SPREZZ)/caribou/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf caribou-$(caribou_UPVER).tar.xz $(TARARGS) $@

.PHONY: cups-pk-helper
cups-pk-helper:$(CUPSPKHELPER)_$(ARCH).deb
$(CUPSPKHELPER): $(SPREZZ)/cups-pk-helper/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf cups-pk-helper-$(cups-pk-helper_UPVER).tar.xz $(TARARGS) $@

.PHONY: epiphany-browser
epiphany-browser:$(EPIPHANYBROWSER)_$(ARCH).deb
$(EPIPHANYBROWSER): $(SPREZZ)/epiphany-browser/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf epiphany-$(epiphany-browser_UPVER).tar.xz $(TARARGS) $@

.PHONY: evince
evince:$(EVINCE)_$(ARCH).deb
$(EVINCE): $(SPREZZ)/evince/debian/changelog $(EVINCEORIG)
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf evince-$(evince_UPVER).tar.xz $(TARARGS) $@

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

.PHONY: evolution-exchange
evolution-exchange:$(EVOLUTIONEXCHANGE)_$(ARCH).deb
$(EVOLUTIONEXCHANGE): $(SPREZZ)/evolution-exchange/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf evolution-exchange-$(evolution-exchange_UPVER).tar.xz $(TARARGS) $@

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

.PHONY: gconf-cleaner
gconf-cleaner:$(GCONFCLEANER)_$(ARCH).deb
$(GCONFCLEANER): $(SPREZZ)/gconf-cleaner/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gconf-cleaner-$(gconf-cleaner_UPVER).tar.gz $(TARARGS) $@

.PHONY: gconf-editor
gconf-editor:$(GCONFEDITOR)_$(ARCH).deb
$(GCONFEDITOR): $(SPREZZ)/gconf-editor/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gconf-editor-$(gconf-editor_UPVER).tar.xz $(TARARGS) $@

.PHONY: gdesklets
gdesklets:$(GDESKLETS)_$(ARCH).deb
$(GDESKLETS): $(SPREZZ)/gdesklets/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gDesklets-$(gdesklets_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: genius
genius:$(GENIUS)_$(ARCH).deb
$(GENIUS): $(SPREZZ)/genius/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf genius-$(genius_UPVER).tar.xz $(TARARGS) $@

.PHONY: ghemical
ghemical:$(GHEMICAL)_$(ARCH).deb
$(GHEMICAL): $(SPREZZ)/ghemical/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf ghemical-$(ghemical_UPVER).tar.gz $(TARARGS) $@

.PHONY: glabels
glabels:$(GLABELS)_$(ARCH).deb
$(GLABELS): $(SPREZZ)/glabels/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf glabels-$(glabels_UPVER).tar.xz $(TARARGS) $@

.PHONY: gmime
gmime:$(GMIME)_$(ARCH).deb
$(GMIME): $(SPREZZ)/gmime/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gmime-$(gmime_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-applets
gnome-applets:$(GNOMEAPPLETS)_$(ARCH).deb
$(GNOMEAPPLETS): $(SPREZZ)/gnome-applets/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-applets-$(gnome-applets_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-bluetooth
gnome-bluetooth:$(GNOMEBLUETOOTH)_$(ARCH).deb
$(GNOMEBLUETOOTH): $(SPREZZ)/gnome-bluetooth/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-bluetooth_$(gnome-bluetooth_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-boxes
gnome-boxes:$(GNOMEBOXES)_$(ARCH).deb
$(GNOMEBOXES): $(SPREZZ)/gnome-boxes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-boxes_$(gnome-boxes_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-colors
gnome-colors:$(GNOMECOLORS)_$(ARCH).deb
$(GNOMECOLORS): $(SPREZZ)/gnome-colors/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnome-colors-$(gnome-colors_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-color-chooser
gnome-color-chooser:$(GNOMECOLORCHOOSER)_$(ARCH).deb
$(GNOMECOLORCHOOSER): $(SPREZZ)/gnome-color-chooser/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnome-color-chooser-$(gnome-color-chooser_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-color-manager
gnome-color-manager:$(GNOMECOLORMANAGER)_$(ARCH).deb
$(GNOMECOLORMANAGER): $(SPREZZ)/gnome-color-manager/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-color-manager_$(gnome-color-manager_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-common
gnome-common:$(GNOMECOMMON)_$(ARCH).deb
$(GNOMECOMMON): $(SPREZZ)/gnome-common/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-common-$(gnome-common_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-contacts
gnome-contacts:$(GNOMECONTACTS)_$(ARCH).deb
$(GNOMECONTACTS): $(SPREZZ)/gnome-contacts/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-contacts-$(gnome-contacts_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-control-center
gnome-control-center:$(GNOMECONTROLCENTER)_$(ARCH).deb
$(GNOMECONTROLCENTER): $(SPREZZ)/gnome-control-center/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-control-center-$(gnome-control-center_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-desktop
gnome-desktop:$(GNOMEDESKTOP)_$(ARCH).deb
$(GNOMEDESKTOP): $(SPREZZ)/gnome-desktop/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-desktop-$(gnome-desktop_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-dictionary
gnome-dictionary:$(GNOMEDICTIONARY)_$(ARCH).deb
$(GNOMEDICTIONARY): $(SPREZZ)/gnome-dictionary/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-dictionary_$(gnome-dictionary_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-disk-utility
gnome-disk-utility:$(GNOMEDISKUTILITY)_$(ARCH).deb
$(GNOMEDISKUTILITY): $(SPREZZ)/gnome-disk-utility/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-disk-utility_$(gnome-disk-utility_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-do
gnome-do:$(GNOMEDO)_$(ARCH).deb
$(GNOMEDO): $(SPREZZ)/gnome-do/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnome-do-$(gnome-do_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-do-plugins
gnome-do-plugins:$(GNOMEDOPLUGINS)_$(ARCH).deb
$(GNOMEDOPLUGINS): $(SPREZZ)/gnome-do-plugins/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnome-do-plugins-$(gnome-do-plugins_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-doc-utils
gnome-doc-utils:$(GNOMEDOCUTILS)_$(ARCH).deb
$(GNOMEDOCUTILS): $(SPREZZ)/gnome-doc-utils/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-doc-utils_$(gnome-doc-utils_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-font-viewer
gnome-font-viewer:$(GNOMEFONTVIEWER)_$(ARCH).deb
$(GNOMEFONTVIEWER): $(SPREZZ)/gnome-font-viewer/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-font-viewer_$(gnome-font-viewer_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-gmail
gnome-gmail:$(GNOMEGMAIL)_$(ARCH).deb
$(GNOMEGMAIL): $(SPREZZ)/gnome-gmail/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnome-gmail-$(gnome-gmail_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-icon-theme-extras
gnome-icon-theme-extras:$(GNOMEICONTHEMEEXTRAS)_$(ARCH).deb
$(GNOMEICONTHEMEEXTRAS): $(SPREZZ)/gnome-icon-theme-extras/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-icon-theme-extras_$(gnome-icon-theme-extras_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-icon-theme-symbolic
gnome-icon-theme-symbolic:$(GNOMEICONTHEMESYMBOLIC)_$(ARCH).deb
$(GNOMEICONTHEMESYMBOLIC): $(SPREZZ)/gnome-icon-theme-symbolic/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-icon-theme-symbolic_$(gnome-icon-theme-symbolic_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-js-common
gnome-js-common:$(GNOMEJSCOMMON)_$(ARCH).deb
$(GNOMEJSCOMMON): $(SPREZZ)/gnome-js-common/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gnome-js-common-$(gnome-js-common_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-keyring
gnome-keyring:$(GNOMEKEYRING)_$(ARCH).deb
$(GNOMEKEYRING): $(SPREZZ)/gnome-keyring/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-keyring_$(gnome-keyring_UPVER).orig.tar.xz $(TARARGS) $@

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

.PHONY: gnome-extra-icons
gnome-extra-icons:$(GNOMEEXTRAICONS)_$(ARCH).deb
$(GNOMEEXTRAICONS): $(SPREZZ)/gnome-extra-icons/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-extra-icons-$(gnome-extra-icons_UPVER).tar.xz $(TARARGS) $@

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

.PHONY: gnome-packagekit
gnome-packagekit:$(GNOMEPACKAGEKIT)_$(ARCH).deb
$(GNOMEPACKAGEKIT): $(SPREZZ)/gnome-packagekit/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-packagekit-$(gnome-packagekit_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-screensaver
gnome-screensaver:$(GNOMESCREENSAVER)_$(ARCH).deb
$(GNOMESCREENSAVER): $(SPREZZ)/gnome-screensaver/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-screensaver-$(gnome-screensaver_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-shell
gnome-shell:$(GNOMESHELL)_$(ARCH).deb
$(GNOMESHELL): $(SPREZZ)/gnome-shell/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-shell-$(gnome-shell_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-shell-extensions
gnome-shell-extensions:$(GNOMESHELLEXTENSIONS)_$(ARCH).deb
$(GNOMESHELLEXTENSIONS): $(SPREZZ)/gnome-shell-extensions/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-shell-extensions-$(gnome-shell-extensions_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-sushi
gnome-sushi:$(GNOMESUSHI)_$(ARCH).deb
$(GNOMESUSHI): $(SPREZZ)/gnome-sushi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-sushi_$(gnome-sushi_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-terminal
gnome-terminal:$(GNOMETERMINAL)_$(ARCH).deb
$(GNOMETERMINAL): $(SPREZZ)/gnome-terminal/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-terminal_$(gnome-terminal_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gnome-themes
gnome-themes:$(GNOMETHEMES)_$(ARCH).deb
$(GNOMETHEMES): $(SPREZZ)/gnome-themes/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gnome-themes_$(gnome-themes_UPVER).orig.tar.bz2 $(TARARGS) $@

.PHONY: gnome-themes-standard
gnome-themes-standard:$(GNOMETHEMESSTANDARD)_$(ARCH).deb
$(GNOMETHEMESSTANDARD): $(SPREZZ)/gnome-themes-standard/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-themes-standard_$(gnome-themes-standard_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: goffice
goffice:$(GOFFICE)_$(ARCH).deb
$(GOFFICE): $(SPREZZ)/goffice/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf goffice-$(goffice_UPVER).tar.xz $(TARARGS) $@

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

.PHONY: gtk-doc
gtk-doc:$(GTKDOC)_$(ARCH).deb
$(GTKDOC): $(SPREZZ)/gtk-doc/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gtk-doc-$(gtk-doc_UPVER).tar.xz $(TARARGS) $@

.PHONY: gtk-sharp
gtk-sharp:$(GTKSHARP2)_$(ARCH).deb
$(GTKSHARP2): $(SPREZZ)/gtk-sharp2/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gtk-sharp-$(gtk-sharp2_UPVER).tar.xz $(TARARGS) $@

.PHONY: gtk2-engines
gtk2-engines:$(GTK2ENGINES)_$(ARCH).deb
$(GTK2ENGINES): $(SPREZZ)/gtk2-engines/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtk-engines-$(gtk2-engines_UPVER).tar.gz $(TARARGS) $@

.PHONY: gtk2-engines-oxygen
gtk2-engines-oxygen:$(GTK2ENGINESOXYGEN)_$(ARCH).deb
$(GTK2ENGINESOXYGEN): $(SPREZZ)/gtk2-engines-oxygen/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf oxygen-gtk2-$(gtk2-engines-oxygen_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: gtk3-engines-unico
gtk3-engines-unico:$(GTK3ENGINESUNICO)_$(ARCH).deb
$(GTK3ENGINESUNICO): $(SPREZZ)/gtk3-engines-unico/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf unico-$(gtk3-engines-unico_UPVER).tar.gz $(TARARGS) $@

.PHONY: oxygen-gtk3
oxygen-gtk3:$(OXYGENGTK3)_$(ARCH).deb
$(OXYGENGTK3): $(SPREZZ)/oxygen-gtk3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf oxygen-gtk3-$(oxygen-gtk3_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgdata
libgdata:$(LIBGDATA)_$(ARCH).deb
$(LIBGDATA): $(SPREZZ)/libgdata/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgdata-$(libgdata_UPVER).tar.xz $(TARARGS) $@

.PHONY: libgnomeui
libgnomeui:$(LIBGNOMEUI)_$(ARCH).deb
$(LIBGNOMEUI): $(SPREZZ)/libgnomeui/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libgnomeui-$(libgnomeui_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgnomeuimm2.6
libgnomeuimm2.6:$(LIBGNOMEUIMM2.6)_$(ARCH).deb
$(LIBGNOMEUIMM2.6): $(SPREZZ)/libgnomeuimm2.6/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xjvf libgnomeuimm-$(libgnomeuimm2.6_UPVER).tar.bz2 $(TARARGS) $@

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

.PHONY: clearlooks-phenix-theme
clearlooks-phenix-theme:$(CLEARLOOKSPHENIXTHEME)_$(ARCH).deb
$(CLEARLOOKSPHENIXTHEME): $(SPREZZ)/clearlooks-phenix-theme/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xzvf clearlooks-phenix-$(clearlooks-phenix-theme_UPVER).tar.gz $(TARARGS) $@

.PHONY: pitivi
pitivi:$(PITIVI)_$(ARCH).deb
$(PITIVI): $(SPREZZ)/pitivi/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf pitivi-$(pitivi_UPVER).tar.xz $(TARARGS) $@

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
	
.PHONY: tomboy
tomboy:$(TOMBOY)_$(ARCH).deb
$(TOMBOY): $(SPREZZ)/tomboy/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf tomboy-$(tomboy_UPVER).tar.xz $(TARARGS) $@

.PHONY: vino
vino:$(VINO)_$(ARCH).deb
$(VINO): $(SPREZZ)/vino/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf vino-$(vino_UPVER).tar.xz $(TARARGS) $@

.PHONY: vte3
vte3:$(VTE3)_$(ARCH).deb
$(VTE3): $(SPREZZ)/vte3/debian/changelog
	mkdir $@
	cp -r $(<D) $@/
	cd $@ && uscan --force-download --download-current-version
	tar xJvf vte_$(vte3_UPVER).orig.tar.xz $(TARARGS) $@
.PHONY: gtkspell
gtkspell:$(GTKSPELL)_$(ARCH).deb
$(GTKSPELL): $(SPREZZ)/gtkspell/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtkspell-$(gtkspell_UPVER).tar.gz $(TARARGS) $@

.PHONY: gtkmm2.4
gtkmm2.4:$(GTKMM2.4)_$(ARCH).deb
$(GTKMM2.4): $(SPREZZ)/gtkmm2.4/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf gtkmm-$(gtkmm2.4_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: librest
librest:$(LIBREST)_$(ARCH).deb
$(LIBREST): $(SPREZZ)/librest/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xjvf rest-$(librest_UPVER).tar.bz2 $(TARARGS) $@

.PHONY: libgxps
libgxps:$(LIBGXPS)_$(ARCH).deb
$(LIBGXPS): $(SPREZZ)/libgxps/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf libgxps-$(libgxps_UPVER).tar.xz $(TARARGS) $@

.PHONY: gnome-games
gnome-games:$(GNOMEGAMES)_$(ARCH).deb
$(GNOMEGAMES): $(SPREZZ)/gnome-games/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-games-$(gnome-games_UPVER).tar.xz $(TARARGS) $@

.PHONY: conduit
conduit:$(CONDUIT)_$(ARCH).deb
$(CONDUIT): $(SPREZZ)/conduit/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf conduit-$(conduit_UPVER).tar.gz $(TARARGS) $@

.PHONY: murrine-themes
murrine-themes:$(MURRINETHEMES)_$(ARCH).deb
$(MURRINETHEMES): $(SPREZZ)/murrine-themes/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf murrine-themes-$(murrine-themes_UPVER).tar.gz $(TARARGS) $@

.PHONY: modemmanager
modemmanager:$(MODEMMANAGER)_$(ARCH).deb
$(MODEMMANAGER): $(SPREZZ)/modemmanager/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf ModemManager-$(modemmanager_UPVER).tar.xz $(TARARGS) $@

.PHONY: gtkgl2
gtkgl2:$(GTKGL2)_$(ARCH).deb
$(GTKGL2): $(SPREZZ)/gtkgl2/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gtkgl2-$(gtkgl2_UPVER).tar.gz $(TARARGS) $@

.PHONY: guile-1.8
guile-1.8:$(GUILE1.8)_$(ARCH).deb
$(GUILE1.8): $(SPREZZ)/guile-1.8/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf guile-$(guile-1.8_UPVER).tar.gz $(TARARGS) $@

.PHONY: gnome-tweak-tool
gnome-tweak-tool:$(GNOMETWEAKTOOL)_$(ARCH).deb
$(GNOMETWEAKTOOL): $(SPREZZ)/gnome-tweak-tool/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gnome-tweak-tool_$(gnome-tweak-tool_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gstreamer0.10
gstreamer0.10:$(GSTREAMER0.10)_$(ARCH).deb
$(GSTREAMER0.10): $(SPREZZ)/gstreamer0.10/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xJvf gstreamer0.10_$(gstreamer0.10_UPVER).orig.tar.xz $(TARARGS) $@

.PHONY: gstreamer0.10-ffmpeg
gstreamer0.10-ffmpeg:$(GSTREAMER0.10FFMPEG)_$(ARCH).deb
$(GSTREAMER0.10FFMPEG): $(SPREZZ)/gstreamer0.10-ffmpeg/debian/changelog
	mkdir $@
	cp -r $(<D) $@
	cd $@ && uscan --force-download --download-current-version
	tar xzvf gstreamer0.10-ffmpeg_$(gstreamer0.10-ffmpeg_UPVER).orig.tar.gz $(TARARGS) $@

